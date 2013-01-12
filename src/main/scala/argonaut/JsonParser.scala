package argonaut

import annotation.tailrec
import scalaz._
import Scalaz._
import Json._
import java.lang.StringBuilder
import scala.collection.mutable.Builder

object JsonParser {
  sealed trait TokenStream {
    def toStream: Stream[JSONToken]
  }
  sealed case class TokenStreamElement(element: JSONToken, next: () => TokenStream) extends TokenStream {
    def toStream: Stream[JSONToken] = Stream.cons(element, next().toStream)
  }
  case object TokenStreamEnd extends TokenStream {
    def toStream: Stream[JSONToken] = Stream.empty
  }

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
  sealed case class UnicodeCharacterToken(codePoint: Int) extends StringPartToken {
    final def originalStringContent = "\\u%04x".format(codePoint)
    final def appendToBuilder(builder: StringBuilder) = builder.appendCodePoint(codePoint)
  }
  sealed case class EscapedCharacterToken(originalStringContent: String, parsedStringContent: String) extends StringPartToken {
    final def appendToBuilder(builder: StringBuilder) = builder.append(parsedStringContent)
  }
  object EscapedCharacterToken {
    val charMap: Map[WrappedCharArray, EscapedCharacterToken] = Map(
      WrappedCharArray.fromString("\\r") -> EscapedCharacterToken("\\r", "\r"),
      WrappedCharArray.fromString("\\n") -> EscapedCharacterToken("\\n", "\n"),
      WrappedCharArray.fromString("\\t") -> EscapedCharacterToken("\\t", "\t"),
      WrappedCharArray.fromString("\\b") -> EscapedCharacterToken("\\b", "\b"),
      WrappedCharArray.fromString("\\f") -> EscapedCharacterToken("\\f", "\f"),
      WrappedCharArray.fromString("""\\""") -> EscapedCharacterToken("""\\""", """\"""),
      WrappedCharArray.fromString("""\/""") -> EscapedCharacterToken("""\/""", """/"""),
      WrappedCharArray.fromString( "\\\"") -> EscapedCharacterToken("\\\"", "\"")
    ) 
  }

  sealed case class NormalStringToken(tokenSource: TokenSource) extends StringPartToken {
    final def originalStringContent = tokenSource.toString
    final def appendToBuilder(builder: StringBuilder) = tokenSource.appendTo(builder)
  }
  sealed case class UnexpectedContentToken(originalStringContent: String) extends JSONToken

  /**
   * Alternative to String implemented similarly to how String was prior to Java 7u6.
   */
  trait WrappedCharArray {
    def isEmpty: Boolean
    def headOption: Option[Char]
    def span(p: Char => Boolean): (WrappedCharArray, WrappedCharArray)
    def take(n: Int): WrappedCharArray
    def drop(n: Int): WrappedCharArray
    def startsWith(possibleStart: String): Boolean
    def tail: WrappedCharArray
    def appendTo(builder: StringBuilder): StringBuilder
    def length: Int
  }

  object WrappedCharArray {
    private[this] case object EmptyWrappedCharArray extends WrappedCharArray {
      val isEmpty = true
      def span(p: (Char) => Boolean) = (this, this)
      val tail = this
      val headOption = None
      def take(n: Int): WrappedCharArray = this
      def drop(n: Int): WrappedCharArray = this
      def startsWith(possibleStart: String): Boolean = possibleStart.isEmpty
      def appendTo(builder: StringBuilder): StringBuilder = builder
      val length = 0
      override def toString(): String = ""
    }

    private[this] case class NonEmptyWrappedCharArray(string: String, start: Int, length: Int) extends WrappedCharArray {
      val isEmpty = false
      def span(p: (Char) => Boolean) = {
        val index = string.indexWhere(char => !p(char), start)
        if (index < 0) (this, EmptyWrappedCharArray)
        else {
          val newLength = index - start
          (take(newLength), drop(newLength))
        }
      }
      def tail = boundedCharArray(string, start + 1, length - 1)
      def headOption = string(start).some
      def take(n: Int): WrappedCharArray = boundedCharArray(string, start, n)
      def drop(n: Int): WrappedCharArray = boundedCharArray(string, start + n, length)
      def startsWith(possibleStart: String): Boolean = string.startsWith(possibleStart, start)
      def appendTo(builder: StringBuilder): StringBuilder = builder.append(string, start, start + length)
      override def toString(): String = string.substring(start, start + length)
      private[this] final lazy val storedHash: Int = {
        var working: Int = 37
        for (i <- start to start + length - 1) {
          val value = string(i)
          working = working * 17 + ((value ^ (value >> 32)))
        }
        working
      }
      override def hashCode() = storedHash
      override def equals(other: Any): Boolean = other match {
        case NonEmptyWrappedCharArray(otherString, otherStart, otherLength) if (length == otherLength) => {
          @tailrec
          def compareChars(positionShift: Int = 0): Boolean = {
            if (positionShift >= length) true
            else if (string(start + positionShift) == otherString(otherStart + positionShift)) compareChars(positionShift + 1)
            else false
          }
          compareChars()
        }
        case _ => false
      }
    }

    private[this] def boundedCharArray(string: String, start: Int, length: Int): WrappedCharArray = {
      if (start >= string.length || length <= 0) EmptyWrappedCharArray
      else {
        val safeLength: Int = if (length + start >= string.length) string.length - start else length
        new NonEmptyWrappedCharArray(string, start, safeLength)
      }
    }

    final def fromString(string: String): WrappedCharArray = {
      boundedCharArray(string, 0, string.length)
    }
  }

  private[this] type TokenSource = WrappedCharArray
  
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

  def parse(json: String): String \/ Json = {
    expectValue(tokenize(json)).flatMap{streamAndValue =>
      streamAndValue match {
        case (TokenStreamEnd, jsonInstance) => jsonInstance.right
        case (tokenStream, _) => "JSON contains invalid suffix content: %s".format(excerpt(tokenStream)).left
      }
    }
  }

  @inline
  private[this] final def expectedSpacerToken(stream: TokenStream, token: JSONToken, failMessage: String): String \/ TokenStream = {
    stream match {
      case TokenStreamElement(`token`, tail) => tail().right
      case _ => "%s but found: %s".format(failMessage, excerpt(stream)).left
    }
  }

  private[this] final def expectStringBounds(stream: TokenStream) = expectedSpacerToken(stream, StringBoundsToken, "Expected string bounds")

  private[this] final def expectEntrySeparator(stream: TokenStream) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  private[this] final def expectFieldSeparator(stream: TokenStream) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")

  @tailrec
  private[this] final def expectObject(stream: TokenStream, first: Boolean = true, fields: InsertionMap[JsonField, Json] = InsertionMap()): String \/ (TokenStream, Json) = {
    stream match {
      case TokenStreamElement(ObjectCloseToken, tail) => (tail(), jObjectMap(fields)).right
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) stream.right[String] else expectEntrySeparator(stream)
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
  private[this] final def expectArray(stream: TokenStream, first: Boolean = true, fields: Builder[Json, List[Json]] = List.newBuilder): String \/ (TokenStream, Json) = {
    stream match {
      case TokenStreamElement(ArrayCloseToken, tail) => (tail(), jArray(fields.result)).right
      case _ => {
        val next = for {
          afterEntrySeparator <- if (first) stream.right[String] else expectEntrySeparator(stream)
          streamAndValue <- expectValue(afterEntrySeparator)
        } yield (streamAndValue._1, fields += streamAndValue._2)
        next match {
          case \/-((newStream, newFields)) => expectArray(newStream, false, newFields)
          case -\/(failure) => failure.left
        }
      }
    }
  }

  private[this] final def expectValue(stream: TokenStream): String \/ (TokenStream, Json) = {
    stream match {
      case TokenStreamElement(ArrayOpenToken, next) => expectArray(next())
      case TokenStreamElement(ObjectOpenToken, next) => expectObject(next())
      case TokenStreamElement(StringBoundsToken, next) => expectStringNoStartBounds(next())
      case TokenStreamElement(BooleanTrueToken, tail) => (tail(), jTrue).right
      case TokenStreamElement(BooleanFalseToken, tail) => (tail(), jFalse).right
      case TokenStreamElement(NullToken, tail) => (tail(), jNull).right
      case TokenStreamElement(NumberToken(numberText), tail) => {
        numberText
          .mkString
          .parseDouble
          .fold(nfe => "Value [%s] cannot be parsed into a number.".format(numberText).left,
                doubleValue => (tail(), jDouble(doubleValue)).right)
      }
      case TokenStreamElement(UnexpectedContentToken(excerpt), _) => "Unexpected content found: %s".format(excerpt).left
      case TokenStreamElement(unexpectedToken, _) => "Unexpected content found: %s".format(excerpt(stream)).left
      case TokenStreamEnd => "JSON terminates unexpectedly".left
    }
  }

  private[this] final def expectString(stream: TokenStream): String \/ (TokenStream, JString) = {
    for {
      afterOpen <- expectStringBounds(stream)
      afterString <- expectStringNoStartBounds(afterOpen)
    } yield afterString
  }

  // Note the mutable collection type in the parameters.
  @tailrec
  private[this] final def collectStringParts(stream: TokenStream, workingTokens: Builder[StringPartToken, List[StringPartToken]] = List.newBuilder): String \/ (TokenStream, Builder[StringPartToken, List[StringPartToken]]) = {
    stream match {
      case TokenStreamElement(stringPartToken: StringPartToken, tail) => collectStringParts(tail(), workingTokens += stringPartToken)
      case _ => (stream, workingTokens).right
    }
  }

  private[this] final val appendStringPartToBuilder = (builder: StringBuilder, part: StringPartToken) => part.appendToBuilder(builder)

  private[this] final def expectStringNoStartBounds(stream: TokenStream): String \/ (TokenStream, JString) = {
    for {
      elements <- collectStringParts(stream)
      afterClose <- expectStringBounds(elements._1)
    } yield (afterClose, JString(elements._2.result.foldLeft(new StringBuilder())(appendStringPartToBuilder).toString))
  }

  private[this] final def unexpectedContent(json: TokenSource): TokenStream = TokenStreamElement(UnexpectedContentToken(json.take(10).toString), () => TokenStreamEnd)

  private[this] final val isNumberChar = (char: Char) => (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.'

  private[this] final def parseNumber(json: TokenSource): Option[(NumberToken, TokenSource)] = {
    val (possibleNumber, remainder) = json.span(isNumberChar)
    if (possibleNumber.isEmpty) None
    else (NumberToken(possibleNumber.toString), remainder).some
  }

  private[this] final val isUnicodeSequenceChar = (char: Char) => {
    (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F') || (char >= '0' && char <= '9')
  }

  private[this] final val isNormalChar = (char: Char) => {
    char != '"' && char != '\\' && !Character.isISOControl(char)
  }

  private[this] final val speechMark: String = """""""
  private[this] final val backslash: String = "\\"
  private[this] final val backslashU: String = "\\u"
  private[this] final val trueArray: String = "true"
  private[this] final val falseArray: String = "false"
  private[this] final val nullArray: String = "null"

  private[this] final def tokenizeString(json: TokenSource): TokenStream = {
    if (json.isEmpty) TokenStreamEnd
    else if (json.startsWith(speechMark)) TokenStreamElement(StringBoundsToken, () => tokenize(json.tail))
    else if (json.startsWith(backslash)) {
      if (json.startsWith(backslashU)) {
        val possibleUnicodeSequence = json.drop(2).take(4)
        if (possibleUnicodeSequence.length == 4) {
          Validation.fromTryCatch(Integer.parseInt(possibleUnicodeSequence.toString, 16)) match {
            case Success(codePoint) => {
              val unicodeCharToken = UnicodeCharacterToken(codePoint)
              TokenStreamElement(unicodeCharToken, () => tokenizeString(json.drop(6)))
            }
            case _ => unexpectedContent(json)
          }
        } else unexpectedContent(json)
      } else {
        EscapedCharacterToken.charMap.get(json.take(2)) match {
          case Some(escapedCharToken) => TokenStreamElement(escapedCharToken, () => tokenizeString(json.drop(2)))
          case _ => unexpectedContent(json)
        }
      }
    } else {
      val (prefix, suffix) = json.span(isNormalChar)
      val normalStringToken = NormalStringToken(prefix)
      suffix.headOption match {
        case Some('\"') | Some('\\') => TokenStreamElement(normalStringToken, () => tokenizeString(suffix))
        case None => TokenStreamElement(normalStringToken, () => TokenStreamEnd)
        case _ => unexpectedContent(suffix)
      }
    }
  }

  final def tokenize(json: String): TokenStream = tokenize(WrappedCharArray.fromString(json))

  final def tokenize(json: TokenSource): TokenStream = {
    json.headOption match {
      case Some('[') => TokenStreamElement(ArrayOpenToken, () => tokenize(json.tail))
      case Some(']') => TokenStreamElement(ArrayCloseToken, () => tokenize(json.tail))
      case Some('{') => TokenStreamElement(ObjectOpenToken, () => tokenize(json.tail))
      case Some('}') => TokenStreamElement(ObjectCloseToken, () => tokenize(json.tail))
      case Some(':') => TokenStreamElement(FieldSeparatorToken, () => tokenize(json.tail))
      case Some(',') => TokenStreamElement(EntrySeparatorToken, () => tokenize(json.tail))
      case Some('"') => TokenStreamElement(StringBoundsToken, () => tokenizeString(json.tail))
      case Some('t') if json.startsWith(trueArray) => TokenStreamElement(BooleanTrueToken, () => tokenize(json.drop(4)))
      case Some('f') if json.startsWith(falseArray) => TokenStreamElement(BooleanFalseToken, () => tokenize(json.drop(5)))
      case Some('n') if json.startsWith(nullArray) => TokenStreamElement(NullToken, () => tokenize(json.drop(4)))
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
