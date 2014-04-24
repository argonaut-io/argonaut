package argonaut

import scala.annotation._
import scalaz._
import Scalaz._
import Json._
import java.lang.StringBuilder
import scala.collection.mutable.Builder

object JsonParser {
  private[this] final type TokenSource = String

  @inline
  private[this] final def excerpt(string: String, position: Int, limit: Int = 50): String = {
    val remaining = string.drop(position)
    if (remaining.size > limit) {
      remaining.take(limit) + "..."
    } else {
      remaining
    }
  }

  final def parse(json: String): String \/ Json = {
    val jsonLength = json.length

    @tailrec
    def validSuffixContent(from: Int): Boolean = {
      if (from >= jsonLength) true
      else json(from) match {
        case ' ' | '\r' | '\n' | '\t' => validSuffixContent(from + 1)
        case _ => false
      }
    }

    def parseResult(result: (Int, Json)): String \/ Json = {
      result match {
        case (`jsonLength`, jsonInstance) => \/-(jsonInstance)
        case (remainder, jsonInstance) if (validSuffixContent(remainder)) => \/-(jsonInstance)
        case (remainder, _) => "JSON contains invalid suffix content: %s".format(excerpt(json, remainder)).left
      }
    }

    expectValue(json, 0).flatMap(parseResult)
  }

  @tailrec
  @inline
  private[this] final def expectedSpacerToken(stream: TokenSource, position: Int, token: Char, failMessage: String): String \/ Int = {
    if (position >= stream.length) "JSON terminates unexpectedly".left
    else stream(position) match {
      case `token` => \/-(position + 1)
      case ' ' | '\r' | '\n' | '\t' => expectedSpacerToken(stream, position + 1, token, failMessage)
      case _ => -\/("%s but found: %s".format(failMessage, excerpt(stream, position)))
    }
  }

  @inline
  private[this] final def expectStringBounds(stream: TokenSource, position: Int) = expectedSpacerToken(stream, position, '"', "Expected string bounds")

  @inline
  private[this] final def expectEntrySeparator(stream: TokenSource, position: Int) = expectedSpacerToken(stream, position, ',', "Expected entry separator token")

  @inline
  private[this] final def expectFieldSeparator(stream: TokenSource, position: Int) = expectedSpacerToken(stream, position, ':', "Expected field separator token")

  @tailrec
  private[this] final def expectObject(stream: TokenSource, position: Int, first: Boolean = true, fields: JsonObject = JsonObject.empty): String \/ (Int, Json) = {
    if (position >= stream.length) "JSON terminates unexpectedly".left
    else stream(position) match {
      case '}' => \/-((position + 1, jObject(fields)))
      case ' ' | '\r' | '\n' | '\t' => expectObject(stream, position + 1, first, fields)
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) \/-(position) else expectEntrySeparator(stream, position)
          streamAndKey <- expectString(stream, afterEntrySeparator)
          afterFieldSeparator <- expectFieldSeparator(stream, streamAndKey._1)
          streamAndValue <- expectValue(stream, afterFieldSeparator)
        } yield (streamAndValue._1, fields + (streamAndKey._2, streamAndValue._2))
        next match {
          case \/-((newPosition, newFields)) => expectObject(stream, newPosition, false, newFields)
          case -\/(failure) => failure.left
        }
      }
    }
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def expectArray(stream: TokenSource, position: Int, first: Boolean = true, fields: Builder[Json, List[Json]] = List.newBuilder): String \/ (Int, Json) = {
    if (position >= stream.length) "JSON terminates unexpectedly".left
    else stream(position) match {
      case ']' => \/-((position + 1, jArray(fields.result)))
      case ' ' | '\r' | '\n' | '\t' => expectArray(stream, position + 1, first, fields)
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) \/-(position) else expectEntrySeparator(stream, position)
          streamAndValue <- expectValue(stream, afterEntrySeparator)
        } yield (streamAndValue._1, fields += streamAndValue._2)
        next match {
          case \/-((newPosition, newFields)) => expectArray(stream, newPosition, false, newFields)
          case -\/(failure) => failure.left
        }
      }
    }
  }

  @tailrec
  private[this] final def expectValue(stream: TokenSource, position: Int): String \/ (Int, Json) = {
    @tailrec
    def safeNumberIndex(index: Int): Int = {
      if (index >= stream.length) stream.length
      else {
        val char = stream(index)
        if ((char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.') safeNumberIndex(index + 1)
        else index
      }
    }

    if (position >= stream.length) "JSON terminates unexpectedly".left
    else stream(position) match {
      case '[' => expectArray(stream, position + 1)
      case '{' => expectObject(stream, position + 1)
      case '"' => expectStringNoStartBounds(stream, position + 1).map(pair => (pair._1, jString(pair._2)))
      case 't' if stream.startsWith("true", position) => \/-((position + 4, jTrue))
      case 'f' if stream.startsWith("false", position) => \/-((position + 5, jFalse))
      case 'n' if stream.startsWith("null", position) => \/-((position + 4, jNull))
      case ' ' | '\r' | '\n' | '\t' => expectValue(stream, position + 1)
      case _ => {
        val numberEndIndex = safeNumberIndex(position)
        if (numberEndIndex == position) unexpectedContent(stream, position)
        else {
          val numberAsString = stream.substring(position, numberEndIndex)
          numberAsString
            .parseDouble
            .fold(nfe => "Value [%s] cannot be parsed into a number.".format(numberAsString).left,
                  doubleValue => \/-((numberEndIndex, jNumberOrNull(doubleValue))))
        }
      }
    }
  }

  @inline
  private[this] final def expectString(stream: TokenSource, position: Int): String \/ (Int, String) = {
    for {
      afterOpen <- expectStringBounds(stream, position)
      afterString <- expectStringNoStartBounds(stream, afterOpen)
    } yield afterString
  }

  @inline
  private[this] final def unexpectedContent[T](stream: TokenSource, position: Int): String \/ T = {
    "Unexpected content found: %s".format(excerpt(stream, position)).left[T]
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def collectStringParts(stream: TokenSource, position: Int, workingString: StringBuilder = new StringBuilder()): String \/ (Int, StringBuilder) = {
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
      if (index >= stream.length) stream.length
      else {
        val char = stream(index)
        if (char == '"' || char == '\\') index
        else safeNormalCharIndex(index + 1)
      }
    }

    if (position >= stream.length) "JSON terminates unexpectedly".left
    else stream(position) match {
      case '"' => \/-((position + 1, workingString))
      case '\\' => {
        if (position + 2 < stream.length) {
          stream(position + 1) match {
            case 'u' if (position + 6 < stream.length) && checkUnicode(position + 2) => {
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
  private[this] final def expectStringNoStartBounds(stream: TokenSource, position: Int): String \/ (Int, String) = {
    for {
      elements <- collectStringParts(stream, position)
    } yield (elements._1, elements._2.toString)
  }
}
