package argonaut

import annotation.tailrec
import scalaz._
import Scalaz._
import Json._
import java.lang.StringBuilder
import scala.collection.mutable.Builder

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
  case object StringBoundsToken extends OpenToken { 
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
    def appendToBuilder(builder: StringBuilder): StringBuilder
  }
  sealed case class UnicodeCharacterToken(unicodeSequence: String) extends StringPartToken {
    final def originalStringContent = "\\u" + unicodeSequence
    final def appendToBuilder(builder: StringBuilder) = builder.appendCodePoint(Integer.valueOf(unicodeSequence, 16))
  }
  sealed case class EscapedCharacterToken(originalStringContent: String, parsedStringContent: String) extends StringPartToken {
    final def appendToBuilder(builder: StringBuilder) = builder.append(parsedStringContent)
  }
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
    final def appendToBuilder(builder: StringBuilder) = builder.append(originalStringContent)
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
  
  private[this] final def expectStringBounds(stream: TokenStream) = expectedSpacerToken(stream, StringBoundsToken, "Expected string bounds")

  private[this] final def expectArrayOpen(stream: TokenStream) = expectedSpacerToken(stream, ArrayOpenToken, "Expected array open token")

  private[this] final def expectArrayClose(stream: TokenStream) = expectedSpacerToken(stream, ArrayCloseToken, "Expected array close token")

  private[this] final def expectObjectOpen(stream: TokenStream) = expectedSpacerToken(stream, ObjectOpenToken, "Expected object open token")

  private[this] final def expectObjectClose(stream: TokenStream) = expectedSpacerToken(stream, ObjectCloseToken, "Expected object close token")

  private[this] final def expectEntrySeparator(stream: TokenStream) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  private[this] final def expectFieldSeparator(stream: TokenStream) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")
  
  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def expectObject(stream: TokenStream, first: Boolean = true, fields: Builder[(JsonField, Json), List[(JsonField, Json)]] = List.newBuilder): ValidationNEL[String, (TokenStream, JObject)] = {
    stream match {
      case TokenStreamElement(ObjectCloseToken, tail) => (tail(), JObject(JsonObject(InsertionMap(fields.result: _*)))).successNel
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
          streamAndKey <- expectString(afterEntrySeparator)
          afterFieldSeperator <- expectFieldSeparator(streamAndKey._1)
          streamAndValue <- expectValue(afterFieldSeperator)
        } yield (streamAndValue._1, fields += ((streamAndKey._2.s, streamAndValue._2)))
        next match {
          case Success((newStream, newFields)) => expectObject(newStream, false, newFields)
          case Failure(failure) => failure.failure
        }
      }
    }
  }
 
  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def expectArray(stream: TokenStream, first: Boolean = true, fields: Builder[Json, List[Json]] = List.newBuilder): ValidationNEL[String, (TokenStream, JArray)] = {
    stream match {
      case TokenStreamElement(ArrayCloseToken, tail) => (tail(), JArray(fields.result)).successNel
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
          streamAndValue <- expectValue(afterEntrySeparator)
        } yield (streamAndValue._1, fields += streamAndValue._2)
        next match {
          case Success((newStream, newFields)) => expectArray(newStream, false, newFields)
          case Failure(failure) => failure.failure
        }
      }
    }
  }

  private[this] final def expectValue(stream: TokenStream): ValidationNEL[String, (TokenStream, Json)] = {
    stream match {
      case TokenStreamElement(ArrayOpenToken, next) => expectArray(next())
      case TokenStreamElement(ObjectOpenToken, next) => expectObject(next())
      case TokenStreamElement(StringBoundsToken, next) => expectStringNoStartBounds(next())
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

  private[this] final def expectString(stream: TokenStream): ValidationNEL[String, (TokenStream, JString)] = {
    for {
      afterOpen <- expectStringBounds(stream)
      afterString <- expectStringNoStartBounds(afterOpen)
    } yield afterString
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def collectStringParts(stream: TokenStream, workingTokens: Builder[StringPartToken, List[StringPartToken]] = List.newBuilder): ValidationNEL[String, (TokenStream, Builder[StringPartToken, List[StringPartToken]])] = {
    stream match {
      case TokenStreamElement(stringPartToken: StringPartToken, tail) => collectStringParts(tail(), workingTokens += stringPartToken)
      case _ => (stream, workingTokens).successNel
    }
  }

  private[this] final val appendStringPartToBuilder = (builder: StringBuilder, part: StringPartToken) => part.appendToBuilder(builder)

  private[this] final def expectStringNoStartBounds(stream: TokenStream): ValidationNEL[String, (TokenStream, JString)] = {
    for {
      elements <- collectStringParts(stream)
      afterClose <- expectStringBounds(elements._1)
    } yield (afterClose, JString(elements._2.result.foldLeft(new StringBuilder())(appendStringPartToBuilder).toString))
  }

  private[this] final def unexpectedContent(json: String): TokenStream = TokenStreamElement(UnexpectedContentToken(json.take(10)), () => TokenStreamEnd)

  private[this] final val isNumberChar = (char: Char) => (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.'

  private[this] final def parseNumber(json: String): Option[(NumberToken, String)] = {
    val (possibleNumber, remainder) = json.span(isNumberChar)
    if (possibleNumber.isEmpty) None
    else (NumberToken(possibleNumber), remainder).some
  }

  private[this] final val isUnicodeSequenceChar = (char: Char) => {
    (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F') || (char >= '0' && char <= '9')
  }

  private[this] final val isNormalChar = (char: Char) => {
    char != '"' && char != '\\' && !Character.isISOControl(char)
  }

  private[this] final def tokenizeString(json: String): TokenStream = {
    if (json.isEmpty) TokenStreamEnd
    else if (json.startsWith(""""""")) TokenStreamElement(StringBoundsToken, () => tokenize(json.tail))
    else if (json.startsWith("""\""")) {
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
  
  final def tokenize(json: String): TokenStream = {
    json.headOption match {
      case Some('[') => TokenStreamElement(ArrayOpenToken, () => tokenize(json.tail))
      case Some(']') => TokenStreamElement(ArrayCloseToken, () => tokenize(json.tail))
      case Some('{') => TokenStreamElement(ObjectOpenToken, () => tokenize(json.tail))
      case Some('}') => TokenStreamElement(ObjectCloseToken, () => tokenize(json.tail))
      case Some(':') => TokenStreamElement(FieldSeparatorToken, () => tokenize(json.tail))
      case Some(',') => TokenStreamElement(EntrySeparatorToken, () => tokenize(json.tail))
      case Some('"') => TokenStreamElement(StringBoundsToken, () => tokenizeString(json.tail))
      case Some('t') if json.startsWith("true") => TokenStreamElement(BooleanTrueToken, () => tokenize(json.drop(4)))
      case Some('f') if json.startsWith("false") => TokenStreamElement(BooleanFalseToken, () => tokenize(json.drop(5)))
      case Some('n') if json.startsWith("null") => TokenStreamElement(NullToken, () => tokenize(json.drop(4)))
      case Some(' ') => tokenize(json.tail)
      case Some('\r') => tokenize(json.tail)
      case Some('\n') => tokenize(json.tail)
      case None => TokenStreamEnd
      case _ => {
        parseNumber(json) match {
          case Some((numberToken, remainder)) => TokenStreamElement(numberToken, () => tokenize(remainder))
          case _ => unexpectedContent(json)
        }
      }
    }
  }
}
