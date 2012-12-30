package argonaut

import annotation.tailrec
import scalaz._
import Scalaz._

object JsonParser {
  sealed trait TokenStream
  sealed case class TokenStreamElement(element: JSONToken, next: () => TokenStream) extends TokenStream
  case object TokenStreamEnd extends TokenStream

  sealed abstract class JSONToken {
    def originalStringContent: String
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
  case object StringBoundsOpenToken extends OpenToken { 
    final val originalStringContent = "\"" 
  }
  case object StringBoundsCloseToken extends CloseToken { 
    final val originalStringContent = "\"" 
  }
  case class NumberToken(originalStringContent: String) extends JSONToken
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
    def parsedStringContent: String
  }
  sealed case class UnicodeCharacterToken(unicodeSequence: String) extends StringPartToken {
    final def originalStringContent = "\\u" + unicodeSequence
    final def parsedStringContent = new java.lang.StringBuilder().appendCodePoint(Integer.valueOf(unicodeSequence, 16)).toString
  }
  sealed case class EscapedCharacterToken(originalStringContent: String, parsedStringContent: String) extends StringPartToken
  object EscapedCharacterToken {
    val charMap: Map[String, EscapedCharacterToken] = Map(
      "\\r" -> EscapedCharacterToken("\\r", "\r"),
      "\\n" -> EscapedCharacterToken("\\n", "\n"),
      "\\t" -> EscapedCharacterToken("\\t", "\t"),
      "\\b" -> EscapedCharacterToken("\\b", "\b"),
      "\\f" -> EscapedCharacterToken("\\f", "\f"),
      """\\""" -> EscapedCharacterToken("""\\""", """\"""),
      """\/""" -> EscapedCharacterToken("""\/""", """/"""),
      "\\\"" -> EscapedCharacterToken("\\\"", "\"")
    ) 
  }

  sealed case class NormalStringToken(originalStringContent: String) extends StringPartToken {
    final def parsedStringContent = originalStringContent
  }
  sealed case class UnexpectedContentToken(originalStringContent: String) extends JSONToken
  
  private[this] def excerpt(string: String, limit: Int = 50): String = {
    if (string.size > limit) {
      string.take(limit) + "..."
    } else {
      string
    }
  }

  private[this] def excerpt(tokens: TokenStream): String = {
    @tailrec
    def getXElements(tokens: TokenStream, elementCount: Int, current: Vector[String] = Vector()): Vector[String] = {
      if (elementCount <= 0) current
      else tokens match {
        case TokenStreamElement(token, tail) => getXElements(tail(), elementCount - 1, current :+ token.originalStringContent)
        case _ => current
      }
    }
    excerpt(getXElements(tokens, 10).mkString)
  }

  def parse(json: String): ValidationNEL[String, Json] = {
    expectValue(tokenize(json)).flatMap{streamAndValue =>
      streamAndValue match {
        case (TokenStreamEnd, jsonInstance) => jsonInstance.successNel
        case (tokenStream, _) => "JSON contains invalid suffix content: %s".format(excerpt(tokenStream)).failNel
      }
    }
  }

  @inline
  private[this] final def expectedSpacerToken(stream: TokenStream, token: JSONToken, failMessage: String): ValidationNEL[String, TokenStream] = {
    stream match {
      case TokenStreamElement(`token`, tail) => tail().successNel
      case _ => "%s but found: %s".format(failMessage, excerpt(stream)).failNel
    }
  }
  
  private[this] final def expectStringOpen(stream: TokenStream) = expectedSpacerToken(stream, StringBoundsOpenToken, "Expected string bounds")

  private[this] final def expectStringClose(stream: TokenStream) = expectedSpacerToken(stream, StringBoundsCloseToken, "Expected string bounds")

  private[this] final def expectArrayOpen(stream: TokenStream) = expectedSpacerToken(stream, ArrayOpenToken, "Expected array open token")

  private[this] final def expectArrayClose(stream: TokenStream) = expectedSpacerToken(stream, ArrayCloseToken, "Expected array close token")

  private[this] final def expectObjectOpen(stream: TokenStream) = expectedSpacerToken(stream, ObjectOpenToken, "Expected object open token")

  private[this] final def expectObjectClose(stream: TokenStream) = expectedSpacerToken(stream, ObjectCloseToken, "Expected object close token")

  private[this] final def expectEntrySeparator(stream: TokenStream) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  private[this] final def expectFieldSeparator(stream: TokenStream) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")
  
  private[this] final def expectObject(stream: TokenStream): ValidationNEL[String, (TokenStream, JObject)] = {
    for {
      afterObjectOpen <- expectObjectOpen(stream)
      streamAndFields <- expectObjectField(true, afterObjectOpen, List.empty)
      mappedVectorAndFields = streamAndFields.copy(_2 = streamAndFields._2.map(pair => (pair._1.s, pair._2)))
    } yield (streamAndFields._1, JObject(JsonObject(InsertionMap(mappedVectorAndFields._2.reverse: _*))))
  }
  
  private[this] final def expectArray(stream: TokenStream): ValidationNEL[String, (TokenStream, JArray)] = {
    for {
      afterArrayOpen <- expectArrayOpen(stream)
      streamAndFields <- expectArrayField(true, afterArrayOpen, List.empty)
    } yield (streamAndFields._1, JArray(streamAndFields._2.reverse))
  }

  private[this] final def expectValue(stream: TokenStream): ValidationNEL[String, (TokenStream, Json)] = {
    stream match {
      case TokenStreamElement(ArrayOpenToken, _) => expectArray(stream)
      case TokenStreamElement(ObjectOpenToken, _) => expectObject(stream)
      case TokenStreamElement(StringBoundsOpenToken, _) => expectString(stream)
      case TokenStreamElement(BooleanTrueToken, tail) => (tail(), JBool(true)).successNel
      case TokenStreamElement(BooleanFalseToken, tail) => (tail(), JBool(false)).successNel
      case TokenStreamElement(NullToken, tail) => (tail(), JNull).successNel
      case TokenStreamElement(NumberToken(numberText), tail) => {
        numberText
          .parseDouble
          .fold(nfe => "Value [%s] cannot be parsed into a number.".format(numberText).failNel,
                doubleValue => (tail(), JNumber(JsonNumber(doubleValue))).successNel)
      }
      case TokenStreamElement(UnexpectedContentToken(excerpt), _) => "Unexpected content found: %s".format(excerpt).failNel
      case TokenStreamElement(unexpectedToken, _) => "Unexpected content found: %s".format(excerpt(stream)).failNel
      case TokenStreamEnd => "JSON terminates unexpectedly".failNel
    }
  }

  @tailrec private[this] final def expectArrayField(first: Boolean, stream: TokenStream, fields: List[Json]): ValidationNEL[String, (TokenStream, List[Json])] = {
    stream match {
      case TokenStreamElement(ArrayCloseToken, tail) => (tail(), fields).successNel
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
          streamAndValue <- expectValue(afterEntrySeparator)
        } yield (streamAndValue._1, streamAndValue._2 :: fields)
        next match {
          case Success((newStream, newFields)) => expectArrayField(false, newStream, newFields)
          case failure => failure
        }
      }
    }
  }
  
  @tailrec private[this] final def expectObjectField(first: Boolean, stream: TokenStream, fields: List[(JString, Json)]): ValidationNEL[String, (TokenStream, List[(JString, Json)])] = {
    stream match {
      case TokenStreamElement(ObjectCloseToken, tail) => (tail(), fields).successNel
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
          streamAndKey <- expectString(afterEntrySeparator)
          afterFieldSeperator <- expectFieldSeparator(streamAndKey._1)
          streamAndValue <- expectValue(afterFieldSeperator)
        } yield (streamAndValue._1, (streamAndKey._2, streamAndValue._2) :: fields)
        next match {
          case Success((newStream, newFields)) => expectObjectField(false, newStream, newFields)
          case failure => failure
        }
      }
    }
  }

  private[this] final def expectString(stream: TokenStream): ValidationNEL[String, (TokenStream, JString)] = {
    @tailrec
    def collectStringParts(stream: TokenStream, workingTokens: Vector[StringPartToken] = Vector.empty): ValidationNEL[String, (TokenStream, Vector[StringPartToken])] = {
      stream match {
        case TokenStreamElement(stringPartToken: StringPartToken, tail) => collectStringParts(tail(), workingTokens :+ stringPartToken)
        case _ => (stream, workingTokens).successNel
      }
    }
    for {
      afterOpen <- expectStringOpen(stream)
      elements <- collectStringParts(afterOpen)
      afterClose <- expectStringClose(elements._1)
    } yield (afterClose, JString(elements._2.foldLeft(new StringBuilder())((builder, part) => builder.append(part.parsedStringContent)).toString))
  }

  private[this] final def unexpectedContent(json: String): TokenStream = TokenStreamElement(UnexpectedContentToken(json.take(10)), () => TokenStreamEnd)

  private[this] final def parseNumber(json: String): Option[(NumberToken, String)] = {
    val (possibleNumber, remainder) = json.span(char => (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.')
    if (possibleNumber.isEmpty) None
    else (NumberToken(possibleNumber), remainder).some
  }

  private[this] final def isUnicodeSequenceChar(char: Char): Boolean = {
    (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F') || (char >= '0' && char <= '9')
  }

  private[this] final def isNormalChar(char: Char): Boolean = {
    !char.isControl && char != '"' && char != '\\'
  }

  private[this] final def tokenizeString(json: String): TokenStream = {
    json.headOption match {
      case Some('"') => {
        TokenStreamElement(StringBoundsCloseToken, () => tokenize(json.tail))
      }
      case None => TokenStreamEnd
      case _ => {
        if (json.startsWith("""\""")) {
          if (json.startsWith("\\u")) {
            val possibleUnicodeSequence = json.drop(2).take(4)
            if (possibleUnicodeSequence.forall(isUnicodeSequenceChar)) {
              val unicodeCharToken = UnicodeCharacterToken(possibleUnicodeSequence)
              TokenStreamElement(unicodeCharToken, () => tokenizeString(json.drop(6)))
            } else unexpectedContent(json)
          } else {
            EscapedCharacterToken.charMap.get(json.take(2)) match {
              case Some(escapedCharToken) => TokenStreamElement(escapedCharToken, () => tokenizeString(json.drop(2)))
              case _ => unexpectedContent(json)
            }
          }
        } else {
          val (prefix: String, suffix: String) = json.span(isNormalChar)
          val normalStringToken = NormalStringToken(prefix)
          suffix.headOption match {
            case Some('\"') | Some('\\') => TokenStreamElement(normalStringToken, () => tokenizeString(suffix))
            case None => TokenStreamElement(normalStringToken, () => TokenStreamEnd)
            case _ => unexpectedContent(suffix)
          }
        }
      }
    }
  }
  
  final def tokenize(json: String): TokenStream = {
    json.headOption match {
      case Some('[') => TokenStreamElement(ArrayOpenToken, () => tokenize(json.tail))
      case Some(']') => TokenStreamElement(ArrayCloseToken, () => tokenize(json.tail))
      case Some('{') => TokenStreamElement(ObjectOpenToken, () => tokenize(json.tail))
      case Some('}') => TokenStreamElement(ObjectCloseToken, () => tokenize(json.tail))
      case Some(':') => TokenStreamElement(FieldSeparatorToken, () => tokenize(json.tail))
      case Some(',') => TokenStreamElement(EntrySeparatorToken, () => tokenize(json.tail))
      case Some('"') => TokenStreamElement(StringBoundsOpenToken, () => tokenizeString(json.tail))
      case Some(' ') => tokenize(json.tail)
      case Some('\r') => tokenize(json.tail)
      case Some('\n') => tokenize(json.tail)
      case None => TokenStreamEnd
      case _ => {
        json match {
          case trueStartingJSON if trueStartingJSON.startsWith("true") => TokenStreamElement(BooleanTrueToken, () => tokenize(json.drop(4)))
          case falseStartingJSON if falseStartingJSON.startsWith("false") => TokenStreamElement(BooleanFalseToken, () => tokenize(json.drop(5)))
          case nullStartingJSON if nullStartingJSON.startsWith("null") => TokenStreamElement(NullToken, () => tokenize(json.drop(4)))
          case _ => {
            parseNumber(json) match {
              case Some((numberToken, remainder)) => TokenStreamElement(numberToken, () => tokenize(remainder))
              case _ => unexpectedContent(json)
            }
          }
        }
      }
    }
  }
}
