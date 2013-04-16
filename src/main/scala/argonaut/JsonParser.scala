package argonaut

import annotation.tailrec
import scalaz._
import Scalaz._
import Json._
import java.lang.StringBuilder
import scala.collection.mutable.Builder

object JsonParser {
  sealed abstract class JSONToken {
    def originalStringContent: String
  }
  case object IgnoreToken extends JSONToken {
    val originalStringContent = ""
  }
  sealed abstract class OpenToken extends JSONToken
  sealed abstract class CloseToken extends JSONToken
  case object ArrayOpenToken extends OpenToken {
    final val originalStringContent = "["
  }
  case object ArrayCloseToken extends CloseToken {
    final val originalStringContent = "]"
  }
  case object ObjectOpenToken extends OpenToken {
    final val originalStringContent = "{"
  }
  case object ObjectCloseToken extends CloseToken {
    final val originalStringContent = "}"
  }
  case object EntrySeparatorToken extends JSONToken {
    final val originalStringContent = ","
  }
  case object FieldSeparatorToken extends JSONToken {
    final val originalStringContent = ":"
  }
  case object StringBoundsToken extends OpenToken {
    final val originalStringContent = "\""
  }
  case class NumberToken(chunk: StringChunk) extends JSONToken {
    def originalStringContent = chunk.getString()
  }
  sealed abstract class BooleanToken extends JSONToken
  case object BooleanTrueToken extends BooleanToken {
    final val originalStringContent = "true"
  }
  case object BooleanFalseToken extends BooleanToken {
    final val originalStringContent = "false"
  }
  case object NullToken extends JSONToken {
    final val originalStringContent = "null"
  }
  sealed abstract class StringPartToken extends JSONToken {
    def appendToBuilder(builder: StringBuilder): StringBuilder
  }
  sealed case class UnicodeCharacterToken(chunk: StringChunk) extends StringPartToken {
    final def originalStringContent = "\\u" + chunk.getString()
    final override def appendToBuilder(builder: StringBuilder): StringBuilder = builder.appendCodePoint(Integer.parseInt(chunk.getString(), 16))
  }
  sealed case class EscapedCharacterToken(originalStringContent: String, parsedStringContent: String) extends StringPartToken {
    final override def appendToBuilder(builder: StringBuilder) = builder.append(parsedStringContent)
  }
  object EscapedCharacterToken {
    val charMap: Map[Char, EscapedCharacterToken] = Map(
      'r' -> EscapedCharacterToken("\\r", "\r"),
      'n' -> EscapedCharacterToken("\\n", "\n"),
      't' -> EscapedCharacterToken("\\t", "\t"),
      'b' -> EscapedCharacterToken("\\b", "\b"),
      'f' -> EscapedCharacterToken("\\f", "\f"),
      '\\' -> EscapedCharacterToken("""\\""", """\"""),
      '/' -> EscapedCharacterToken("""\/""", """/"""),
      '"' -> EscapedCharacterToken("\\\"", "\"")
    )
  }

  sealed case class NormalStringToken(chunk: StringChunk) extends StringPartToken {
    final def originalStringContent = chunk.getString()
    final def appendToBuilder(builder: StringBuilder) = chunk.appendToBuilder(builder)
  }
  sealed case class UnexpectedContentToken(chunk: StringChunk) extends JSONToken {
    final def originalStringContent = chunk.getString()
  }

  private[this] final type TokenSource = String

  private[this] final def excerpt(string: String, limit: Int = 50): String = {
    if (string.size > limit) {
      string.take(limit) + "..."
    } else {
      string
    }
  }

  private[this] final def excerpt(tokens: List[JSONToken]): String = {
    @tailrec
    def getXElements(tokens: List[JSONToken], elementCount: Int, current: Vector[String] = Vector()): Vector[String] = {
      if (elementCount <= 0) current
      else tokens match {
        case token :: tail => getXElements(tail, elementCount - 1, current :+ token.originalStringContent)
        case _ => current
      }
    }
    excerpt(getXElements(tokens, 10).mkString)
  }

  final def parse(json: String): String \/ Json = {
    @tailrec
    def parseResult(result: (List[JSONToken], Json)): String \/ Json = {
      result match {
        case (Nil, jsonInstance) => \/-(jsonInstance)
        case (IgnoreToken :: tail, jsonInstance) => parseResult((tail, jsonInstance))
        case (list, _) => "JSON contains invalid suffix content: %s".format(excerpt(list)).left
      }
    }

    expectValue(processSource(json)).flatMap(parseResult)
  }

  @tailrec
  private[this] final def expectedSpacerToken(stream: List[JSONToken], token: JSONToken, failMessage: String): String \/ List[JSONToken] = {
    stream match {
      case `token` :: tail => \/-(tail)
      case IgnoreToken :: tail => expectedSpacerToken(tail, token, failMessage)
      case _ => -\/("%s but found: %s".format(failMessage, excerpt(stream)))
    }
  }

  private[this] final def expectStringBounds(stream: List[JSONToken]) = expectedSpacerToken(stream, StringBoundsToken, "Expected string bounds")

  private[this] final def expectEntrySeparator(stream: List[JSONToken]) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  private[this] final def expectFieldSeparator(stream: List[JSONToken]) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")

  @tailrec
  private[this] final def expectObject(stream: List[JSONToken], first: Boolean = true, fields: InsertionMap[JsonField, Json] = InsertionMap()): String \/ (List[JSONToken], Json) = {
    stream match {
      case ObjectCloseToken :: tail => \/-((tail, jObjectMap(fields)))
      case IgnoreToken :: tail => expectObject(tail, first, fields)
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) \/-(stream) else expectEntrySeparator(stream)
          streamAndKey <- expectString(afterEntrySeparator)
          afterFieldSeparator <- expectFieldSeparator(streamAndKey._1)
          streamAndValue <- expectValue(afterFieldSeparator)
        } yield (streamAndValue._1, fields ^+^ (streamAndKey._2.s, streamAndValue._2))
        next match {
          case \/-((newStream, newFields)) => expectObject(newStream, false, newFields)
          case -\/(failure) => failure.left
        }
      }
    }
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def expectArray(stream: List[JSONToken], first: Boolean = true, fields: Builder[Json, List[Json]] = List.newBuilder): String \/ (List[JSONToken], Json) = {
    stream match {
      case ArrayCloseToken :: tail => \/-((tail, jArray(fields.result)))
      case IgnoreToken :: tail => expectArray(tail, first, fields)
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) \/-(stream) else expectEntrySeparator(stream)
          streamAndValue <- expectValue(afterEntrySeparator)
        } yield (streamAndValue._1, fields += streamAndValue._2)
        next match {
          case \/-((newStream, newFields)) => expectArray(newStream, false, newFields)
          case -\/(failure) => failure.left
        }

      }
    }
  }

  @tailrec
  private[this] final def expectValue(stream: List[JSONToken]): String \/ (List[JSONToken], Json) = {
    stream match {
      case ArrayOpenToken :: next => expectArray(next)
      case ObjectOpenToken :: next => expectObject(next)
      case StringBoundsToken :: next => expectStringNoStartBounds(next)
      case BooleanTrueToken :: tail => \/-((tail, jTrue))
      case BooleanFalseToken :: tail => \/-((tail, jFalse))
      case NullToken :: tail => \/-((tail, jNull))
      case NumberToken(numberText) :: tail => {
        val numberAsString = numberText.getString()
        numberAsString
          .parseDouble
          .fold(nfe => "Value [%s] cannot be parsed into a number.".format(numberAsString).left,
                doubleValue => \/-((tail, jNumber(doubleValue))))
      }
      case IgnoreToken :: tail => expectValue(tail)
      case UnexpectedContentToken(excerpt) :: _ => "Unexpected content found: %s".format(excerpt.getString()).left
      case unexpectedToken :: _ => "Unexpected content found: %s".format(excerpt(stream)).left
      case Nil => "JSON terminates unexpectedly".left
    }
  }

  private[this] final def expectString(stream: List[JSONToken]): String \/ (List[JSONToken], JString) = {
    for {
      afterOpen <- expectStringBounds(stream)
      afterString <- expectStringNoStartBounds(afterOpen)
    } yield afterString
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def collectStringParts(stream: List[JSONToken], workingTokens: Builder[StringPartToken, List[StringPartToken]] = List.newBuilder): String \/ (List[JSONToken], Builder[StringPartToken, List[StringPartToken]]) = {
    stream match {
      case (stringPartToken: StringPartToken) :: tail => collectStringParts(tail, workingTokens += stringPartToken)
      case _ => \/-((stream, workingTokens))
    }
  }

  private[this] final val appendStringPartToBuilder = (builder: StringBuilder, part: StringPartToken) => part.appendToBuilder(builder)

  private[this] final def expectStringNoStartBounds(stream: List[JSONToken]): String \/ (List[JSONToken], JString) = {
    for {
      elements <- collectStringParts(stream)
      afterClose <- expectStringBounds(elements._1)
    } yield (afterClose, JString(elements._2.result.foldLeft(new StringBuilder())(appendStringPartToBuilder).toString))
  }

  private[this] final val isNotNumberChar = (char: Char) => !((char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.')

  private[this] final val isNotNormalChar = (char: Char) => {
    !(char != '"' && char != '\\' && !Character.isISOControl(char))
  }

  private[this] final def isUnicodeSequenceChar(char: Char) = {
    (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F') || (char >= '0' && char <= '9')
  }

  case class StringChunk(string: TokenSource, start: Int, end: Int) {
    def appendToBuilder(builder: StringBuilder): StringBuilder = builder.append(string, start, end)

    final def getString(): String = string.substring(start, end)
  }

  sealed abstract class ProcessMode
  case object NormalMode extends ProcessMode
  case object StringMode extends ProcessMode

  def processSource(json: TokenSource): List[JSONToken] = {
    val length = json.length
    @tailrec
    def process(mode: ProcessMode = NormalMode,
                position: Int = 0,
                workingTokens: Builder[JSONToken, List[JSONToken]] = List.newBuilder,
                toAdd: JSONToken = IgnoreToken): List[JSONToken] = {
      workingTokens += toAdd

      def unexpectedContent(): List[JSONToken] = {
        workingTokens += UnexpectedContentToken(StringChunk(json, position, (position + 10) min length))
        workingTokens.result()
      }

      @inline
      def finish(): List[JSONToken] = workingTokens.result().drop(1)

      @tailrec
      def checkUnicode(from: Int, unicodeShift: Int = 0): Boolean = {
        if (unicodeShift >= 4) true
        else if (isUnicodeSequenceChar(json(from + unicodeShift))) checkUnicode(from, unicodeShift + 1)
        else false
      }

      @tailrec
      def safeIndexWhere(index: Int, predicate: (Char) => Boolean): Int = {
        if (index >= length) length
        else if (predicate(json(index))) index
        else safeIndexWhere(index + 1, predicate)
      }

      if (position >= length) finish()
      else {
        mode match {
          case StringMode => {
            json(position) match {
              case '"' => process(NormalMode, position + 1, workingTokens, StringBoundsToken)
              case '\\' => {
                if (position + 2 < length) {
                  json(position + 1) match {
                    case 'u' if (position + 6 < length) && checkUnicode(position + 2) => {
                      process(StringMode, position + 6, workingTokens, UnicodeCharacterToken(StringChunk(json, position + 2, position + 6)))
                    }
                    case 'u' => unexpectedContent()
                    case otherChar => EscapedCharacterToken.charMap.get(otherChar) match {
                      case Some(escapedCharToken) => process(StringMode, position + 2, workingTokens, escapedCharToken)
                      case _ => unexpectedContent()
                    }
                  }
                } else unexpectedContent()
              }
              case controlChar if (Character.isISOControl(controlChar)) => {
                process(StringMode, position + 1, workingTokens, UnicodeCharacterToken(StringChunk(json, position, position + 1)))
              }
              case other => {
                val normalCharEnd = safeIndexWhere(position, isNotNormalChar)
                process(StringMode, normalCharEnd, workingTokens, NormalStringToken(StringChunk(json, position, normalCharEnd)))
              }
            }
          }
          case NormalMode => {
            json(position) match {
              case '[' => process(NormalMode, position + 1, workingTokens, ArrayOpenToken)
              case ']' => process(NormalMode, position + 1, workingTokens, ArrayCloseToken)
              case '{' => process(NormalMode, position + 1, workingTokens, ObjectOpenToken)
              case '}' => process(NormalMode, position + 1, workingTokens, ObjectCloseToken)
              case ':' => process(NormalMode, position + 1, workingTokens, FieldSeparatorToken)
              case ',' => process(NormalMode, position + 1, workingTokens, EntrySeparatorToken)
              case '"' => process(StringMode, position + 1, workingTokens, StringBoundsToken)
              case 't' if json.startsWith("true", position) => process(NormalMode, position + 4, workingTokens, BooleanTrueToken)
              case 'f' if json.startsWith("false", position) => process(NormalMode, position + 5, workingTokens, BooleanFalseToken)
              case 'n' if json.startsWith("null", position) => process(NormalMode, position + 4, workingTokens, NullToken)
              case ' ' => process(NormalMode, position + 1, workingTokens, IgnoreToken)
              case '\r' => process(NormalMode, position + 1, workingTokens, IgnoreToken)
              case '\n' => process(NormalMode, position + 1, workingTokens, IgnoreToken)
              case '\t' => process(NormalMode, position + 1, workingTokens, IgnoreToken)
              case _ => {
                val numberEndIndex = safeIndexWhere(position, isNotNumberChar)
                if (numberEndIndex == position) unexpectedContent()
                else {
                  process(NormalMode, numberEndIndex, workingTokens, NumberToken(StringChunk(json, position, numberEndIndex)))
                }
              }
            }
          }
        }
      }
    }

    process()
  }
}
