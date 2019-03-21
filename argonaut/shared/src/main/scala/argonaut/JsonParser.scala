package argonaut

import scala.annotation.tailrec
import Json._
import java.lang.StringBuilder
import scala.collection.mutable.Builder

object JsonParser {
  private[this] final type TokenSource = String

  private[this] val unexpectedTermination = Left("JSON terminates unexpectedly.")

  private[this] case class JsonObjectBuilder(
    var fieldsMapBuilder: Builder[(JsonField, Json), Map[JsonField, Json]] = Map.newBuilder,
    var orderedFieldsBuilder: Builder[JsonField, Vector[JsonField]] = Vector.newBuilder) {
    private[this] var isEmpty: Boolean = true

    def add(key: JsonField, value: Json): JsonObjectBuilder = {
      isEmpty = false
      fieldsMapBuilder += ((key, value))
      orderedFieldsBuilder += key
      this
    }

    def build(): Json = {
      if (isEmpty) jEmptyObject
      else jObject(JsonObjectInstance(fieldsMapBuilder.result(), orderedFieldsBuilder.result()))
    }
  }

  @inline
  private[this] final def excerpt(string: String, position: Int, limit: Int = 50): String = {
    val remaining = string.drop(position)
    if (remaining.length > limit) {
      remaining.take(limit) + "..."
    } else {
      remaining
    }
  }

  final def parse(json: String): Either[String, Json] = {
    val jsonLength = json.length

    @tailrec
    def validSuffixContent(from: Int): Boolean = {
      if (from >= jsonLength) true
      else json(from) match {
        case ' ' | '\r' | '\n' | '\t' => validSuffixContent(from + 1)
        case _ => false
      }
    }

    def parseResult(result: (Int, Json)): Either[String, Json] = {
      result match {
        case (`jsonLength`, jsonInstance) => Right(jsonInstance)
        case (remainder, jsonInstance) if (validSuffixContent(remainder)) => Right(jsonInstance)
        case (remainder, _) => Left("JSON contains invalid suffix content: " + excerpt(json, remainder))
      }
    }

    expectValue(json, 0).right.flatMap(parseResult)
  }

  @tailrec
  @inline
  private[this] final def expectedSpacerToken(stream: TokenSource, position: Int, token: Char, failMessage: String): Either[String, Int] = {
    if (position >= stream.length) unexpectedTermination
    else stream(position) match {
      case `token` => Right(position + 1)
      case ' ' | '\r' | '\n' | '\t' => expectedSpacerToken(stream, position + 1, token, failMessage)
      case _ => Left(s"${failMessage} but found: ${excerpt(stream, position)}")
    }
  }

  @inline
  private[this] final def expectStringBounds(stream: TokenSource, position: Int) = expectedSpacerToken(stream, position, '"', "Expected string bounds")

  @inline
  private[this] final def expectEntrySeparator(stream: TokenSource, position: Int) = expectedSpacerToken(stream, position, ',', "Expected entry separator token")

  @inline
  private[this] final def expectFieldSeparator(stream: TokenSource, position: Int) = expectedSpacerToken(stream, position, ':', "Expected field separator token")

  @tailrec
  private[this] final def expectObject(stream: TokenSource, position: Int, first: Boolean = true, fields: JsonObjectBuilder = new JsonObjectBuilder()): Either[String, (Int, Json)] = {
    if (position >= stream.length) unexpectedTermination
    else stream(position) match {
      case '}' => Right((position + 1, fields.build()))
      case ' ' | '\r' | '\n' | '\t' => expectObject(stream, position + 1, first, fields)
      case _ => {
        val next = for {
          afterEntrySeparator <- (if (first) Right(position) else expectEntrySeparator(stream, position)).right
          streamAndKey <- expectString(stream, afterEntrySeparator).right
          afterFieldSeparator <- expectFieldSeparator(stream, streamAndKey._1).right
          streamAndValue <- expectValue(stream, afterFieldSeparator).right
        } yield (streamAndValue._1, fields.add(streamAndKey._2, streamAndValue._2))
        next match {
          case Right((newPosition, newFields)) => expectObject(stream, newPosition, false, newFields)
          case Left(failure) => Left(failure)
        }
      }
    }
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def expectArray(stream: TokenSource, position: Int, first: Boolean = true, fields: Builder[Json, List[Json]] = List.newBuilder): Either[String, (Int, Json)] = {
    if (position >= stream.length) unexpectedTermination
    else stream(position) match {
      case ']' => Right((position + 1, jArray(fields.result)))
      case ' ' | '\r' | '\n' | '\t' => expectArray(stream, position + 1, first, fields)
      case _ => {
        val next = for {
          afterEntrySeparator <- (if (first) Right(position) else expectEntrySeparator(stream, position)).right
          streamAndValue <- expectValue(stream, afterEntrySeparator).right
        } yield (streamAndValue._1, fields += streamAndValue._2)
        next match {
          case Right((newPosition, newFields)) => expectArray(stream, newPosition, false, newFields)
          case Left(failure) => Left(failure)
        }
      }
    }
  }

  @tailrec
  private[this] final def expectValue(stream: TokenSource, position: Int): Either[String, (Int, Json)] = {

    @tailrec
    def safeNumberIndex(index: Int): Int = {
      if (index >= stream.length) stream.length
      else {
        val char = stream(index)
        if ((char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.') safeNumberIndex(index + 1)
        else index
      }
    }

    if (position >= stream.length) unexpectedTermination
    else stream(position) match {
      case '[' => expectArray(stream, position + 1)
      case '{' => expectObject(stream, position + 1)
      case '"' => expectStringNoStartBounds(stream, position + 1).right.map(pair => (pair._1, jString(pair._2)))
      case 't' if stream.startsWith("true", position) => Right((position + 4, jTrue))
      case 'f' if stream.startsWith("false", position) => Right((position + 5, jFalse))
      case 'n' if stream.startsWith("null", position) => Right((position + 4, jNull))
      case ' ' | '\r' | '\n' | '\t' => expectValue(stream, position + 1)
      case _ => {
        val numberEndIndex = safeNumberIndex(position)
        if (numberEndIndex == position) unexpectedContent(stream, position)
        else {
          val numberAsString = stream.substring(position, numberEndIndex)
          JsonNumber.fromString(numberAsString) match {
            case Some(jn) => Right((numberEndIndex, jn.asJson))
            case None => Left(s"Value [${numberAsString}] cannot be parsed into a number.")
          }
        }
      }
    }
  }

  @inline
  private[this] final def expectString(stream: TokenSource, position: Int): Either[String, (Int, String)] = {
    for {
      afterOpen <- expectStringBounds(stream, position).right
      afterString <- expectStringNoStartBounds(stream, afterOpen).right
    } yield afterString
  }

  @inline
  private[this] final def unexpectedContent[T](stream: TokenSource, position: Int): Either[String, T] = {
    Left("Unexpected content found: " + excerpt(stream, position))
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def collectStringParts(stream: TokenSource, position: Int, workingString: StringBuilder = new StringBuilder()): Either[String, (Int, StringBuilder)] = {
    val streamLength = stream.length

    @tailrec
    @inline
    def checkUnicode(from: Int, unicodeShift: Int = 0): Boolean = {
      if (unicodeShift >= 4) true
      else {
        val char = stream(from + unicodeShift)
        if ((char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F') || (char >= '0' && char <= '9')) checkUnicode(from, unicodeShift + 1)
        else false
      }
    }

    @tailrec
    @inline
    def safeNormalCharIndex(index: Int): Int = {
      if (index >= streamLength) streamLength
      else {
        val char = stream(index)
        if (char == '"' || char == '\\') index
        else safeNormalCharIndex(index + 1)
      }
    }

    if (position >= streamLength) unexpectedTermination
    else stream(position) match {
      case '"' => Right((position + 1, workingString))
      case '\\' => {
        if (position + 2 < streamLength) {
          stream(position + 1) match {
            case 'u' if (position + 6 < streamLength) && checkUnicode(position + 2) => {
              collectStringParts(stream, position + 6, workingString.appendCodePoint(Integer.parseInt(stream.substring(position + 2, position + 6), 16)))
            }
            case 'u' => unexpectedContent(stream, position)
            case 'r' => collectStringParts(stream, position + 2, workingString.append("\r"))
            case 'n' => collectStringParts(stream, position + 2, workingString.append("\n"))
            case 't' => collectStringParts(stream, position + 2, workingString.append("\t"))
            case 'b' => collectStringParts(stream, position + 2, workingString.append("\b"))
            case 'f' => collectStringParts(stream, position + 2, workingString.append("\f"))
            case '\\' => collectStringParts(stream, position + 2, workingString.append("""\"""))
            case '/' => collectStringParts(stream, position + 2, workingString.append("""/"""))
            case '"' => collectStringParts(stream, position + 2, workingString.append("\""))
            case _ => unexpectedContent(stream, position)
          }
        } else unexpectedContent(stream, position)
      }
      case other => {
        val normalCharEnd = safeNormalCharIndex(position)
        collectStringParts(stream, normalCharEnd, workingString.append(stream, position, normalCharEnd))
      }
    }
  }

  @inline
  private[this] final def expectStringNoStartBounds(stream: TokenSource, position: Int): Either[String, (Int, String)] = {
    for {
      elements <- collectStringParts(stream, position).right
    } yield (elements._1, elements._2.toString())
  }
}
