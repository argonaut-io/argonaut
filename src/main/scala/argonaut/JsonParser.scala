package argonaut

import annotation.tailrec
import scalaz._
import Scalaz._

object JsonParser {
  sealed abstract class JSONToken {
    def originalStringContent: String
  }
  abstract class OpenToken extends JSONToken
  abstract class CloseToken extends JSONToken
  case object ArrayOpenToken extends OpenToken { val originalStringContent = "[" }
  case object ArrayCloseToken extends CloseToken { val originalStringContent = "]" }
  case object ObjectOpenToken extends OpenToken { val originalStringContent = "{" }
  case object ObjectCloseToken extends CloseToken { val originalStringContent = "}" }
  case object EntrySeparatorToken extends JSONToken { val originalStringContent = "," }
  case object FieldSeparatorToken extends JSONToken { val originalStringContent = ":" }
  case object StringBoundsOpenToken extends OpenToken { val originalStringContent = "\"" }
  case object StringBoundsCloseToken extends CloseToken { val originalStringContent = "\"" }
  case class NumberToken(originalStringContent: String) extends JSONToken
  abstract class BooleanToken extends JSONToken
  case object BooleanTrueToken extends BooleanToken { val originalStringContent = "true" }
  case object BooleanFalseToken extends BooleanToken { val originalStringContent = "false" }
  case object NullToken extends JSONToken { val originalStringContent = "null" }
  abstract class StringPartToken extends JSONToken {
    def parsedStringContent: String
  }
  case class UnicodeCharacterToken(originalStringContent: String) extends StringPartToken {
    def parsedStringContent = new java.lang.StringBuilder().appendCodePoint(Integer.valueOf(originalStringContent.takeRight(4), 16)).toString
  }
  case class EscapedCharacterToken(originalStringContent: String) extends StringPartToken {
    def parsedStringContent = originalStringContent match {
      case "\\r" => "\r"
      case "\\n" => "\n"
      case "\\t" => "\t"
      case "\\b" => "\b"
      case "\\f" => "\f"
      case """\\""" => """\"""
      case """\/""" => """/"""
      case "\\\"" => "\""
      case x => x.tail
    }
  }
  case class NormalStringToken(originalStringContent: String) extends StringPartToken {
    def parsedStringContent = originalStringContent
  }
  case class UnexpectedContentToken(originalStringContent: String) extends JSONToken
  
  val UnicodeCharRegex = """(\\u[a-fA-F0-9]{4})""".r
  val EscapedCharRegex = """(\\[\\/bfnrt"])""".r
  val NormalStringRegex = """([^\"[\x00-\x1F]\\]+)""".r
  val NumberRegex = """(-?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?)""".r

  private[this] def excerpt(string: String, limit: Int = 50): String = {
    if (string.size > limit) {
      string.take(limit) + "..."
    } else {
      string
    }
  }

  private[this] def excerpt(tokens: List[JSONToken]): String = excerpt(tokens.map(_.originalStringContent).mkString)

  def parse(json: String): ValidationNEL[String, Json] = {
    expectValue(tokenize(json).toList).flatMap{streamAndValue =>
      if (streamAndValue._1.isEmpty) {
        streamAndValue._2.successNel
      } else {
        "JSON contains invalid suffix content: %s".format(excerpt(streamAndValue._1)).failNel
      }
    }
  }

  def tokenize(json: String): Vector[JSONToken] = tokenize(none, json)

  @inline
  def expectedSpacerToken(stream: List[JSONToken], token: JSONToken, failMessage: String): ValidationNEL[String, List[JSONToken]] = {
    stream.headOption match {
      case Some(`token`) => stream.tail.successNel
      case _ => "%s but found: %s".format(failMessage, excerpt(stream)).failNel
    }
  }
  
  @inline
  def expectStringOpen(stream: List[JSONToken]) = expectedSpacerToken(stream, StringBoundsOpenToken, "Expected string bounds")

  @inline
  def expectStringClose(stream: List[JSONToken]) = expectedSpacerToken(stream, StringBoundsCloseToken, "Expected string bounds")

  @inline
  def expectArrayOpen(stream: List[JSONToken]) = expectedSpacerToken(stream, ArrayOpenToken, "Expected array open token")

  @inline
  def expectArrayClose(stream: List[JSONToken]) = expectedSpacerToken(stream, ArrayCloseToken, "Expected array close token")

  @inline
  def expectObjectOpen(stream: List[JSONToken]) = expectedSpacerToken(stream, ObjectOpenToken, "Expected object open token")

  @inline
  def expectObjectClose(stream: List[JSONToken]) = expectedSpacerToken(stream, ObjectCloseToken, "Expected object close token")

  @inline
  def expectEntrySeparator(stream: List[JSONToken]) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  @inline
  def expectFieldSeparator(stream: List[JSONToken]) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")
  
  def expectObject(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], JObject)] = {
    for {
      afterObjectOpen <- expectObjectOpen(stream)
      streamAndFields <- expectObjectField(true, (afterObjectOpen, Vector[(JString, Json)]()).successNel)
      mappedVectorAndFields = streamAndFields.copy(_2 = streamAndFields._2.map(pair => (pair._1.s, pair._2)))
    } yield (streamAndFields._1, JObject(JsonObject(InsertionMap(mappedVectorAndFields._2: _*))))
  }
  
  def expectArray(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], JArray)] = {
    for {
      afterArrayOpen <- expectArrayOpen(stream)
      streamAndFields <- expectArrayField(true, (afterArrayOpen, Vector[Json]()).successNel)
    } yield (streamAndFields._1, JArray(streamAndFields._2.toList))
  }

  def expectValue(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], Json)] = {
    stream match {
      case ArrayOpenToken :: _ => expectArray(stream)
      case ObjectOpenToken :: _ => expectObject(stream)
      case StringBoundsOpenToken :: _ => expectString(stream)
      case BooleanTrueToken :: tail => (tail, JBool(true)).successNel
      case BooleanFalseToken :: tail => (tail, JBool(false)).successNel
      case NullToken :: tail => (tail, JNull).successNel
      case NumberToken(numberText) :: tail => {
        numberText
          .parseDouble
          .fold(nfe => "Value [%s] cannot be parsed into a number.".format(numberText).failNel,
                doubleValue => (tail, JNumber(JsonNumber(doubleValue))).successNel)
      }
      case UnexpectedContentToken(excerpt) :: _ => "Unexpected content found: %s".format(excerpt).failNel
      case unexpectedToken :: _ => "Unexpected content found: %s".format(excerpt(stream)).failNel
      case Nil => "JSON terminates unexpectedly".failNel
    }
  }

  @tailrec def expectArrayField(first: Boolean, currentVector: ValidationNEL[String, (List[JSONToken], Seq[Json])]): ValidationNEL[String, (List[JSONToken], Seq[Json])] = {
    currentVector match {
      case Success((stream, fields)) => {
        stream.headOption match {
          case Some(ArrayCloseToken) => (stream.tail, fields).successNel
          case _ => {
            expectArrayField(false, for {
              afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
              streamAndValue <- expectValue(afterEntrySeparator)
            } yield (streamAndValue._1, fields :+ streamAndValue._2))
          }
        }
      }
      case _ => currentVector
    }
  }
  
  @tailrec def expectObjectField(first: Boolean, currentVector: ValidationNEL[String, (List[JSONToken], Seq[(JString, Json)])]): ValidationNEL[String, (List[JSONToken], Seq[(JString, Json)])] = {
    currentVector match {
      case Success((stream, fields)) => {
        stream match {
          case ObjectCloseToken :: tail => (tail, fields).successNel
          case _ => {
            expectObjectField(false, for {
              afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
              streamAndKey <- expectString(afterEntrySeparator)
              afterFieldSeperator <- expectFieldSeparator(streamAndKey._1)
              streamAndValue <- expectValue(afterFieldSeperator)
            } yield (streamAndValue._1, fields :+ (streamAndKey._2, streamAndValue._2)))
          }
        }
      }
      case _ => currentVector
    }
  }

  def expectString(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], JString)] = {
    for {
      afterOpen <- expectStringOpen(stream)
      elements <- afterOpen.span(jsonToken => jsonToken.isInstanceOf[StringPartToken]).successNel[String]
      afterClose <- expectStringClose(elements._2)
    } yield (afterClose, JString(elements._1.collect{case stringPart: StringPartToken => stringPart.parsedStringContent}.mkString))
  }

  @inline def unexpectedContent(json: String) = Vector(UnexpectedContentToken(json.take(10)))

  /*
  @inline def parseStringSegments(json: String): Vector[JSONToken] = {
    if (json.head == '"') {
      streamCons(StringBoundsCloseToken, json.tail)
    } else {
      NormalStringRegex.findPrefixOf(json).map(normalStringContent => streamCons(NormalStringToken(normalStringContent), json.drop(normalStringContent.length)))
      .orElse(EscapedCharRegex.findPrefixOf(json).map(escapedChar => streamCons(EscapedCharacterToken(escapedChar), json.drop(escapedChar.length))))
      .orElse(UnicodeCharRegex.findPrefixOf(json).map(unicodeContent => streamCons(UnicodeCharacterToken(unicodeContent), json.drop(unicodeContent.length))))
      .getOrElse(unexpectedContent(json))
    }
  }

  @inline def streamCons(token: JSONToken, jsonRemainder: String): Vector[JSONToken] = token +: tokenize(token.some, jsonRemainder)
  */
  
  @tailrec private[this] def tokenize(previousToken: Option[JSONToken], json: String, current: Vector[JSONToken] = Vector.empty): Vector[JSONToken] = {
    if (json.isEmpty) current
    else {
      previousToken match {
        case Some(StringBoundsOpenToken) | Some(_: StringPartToken) => {
          if (json.head == '"') {
            tokenize(StringBoundsCloseToken.some, json.tail, current :+ StringBoundsCloseToken)
          } else if (json.startsWith("""\""")) {
            if (json.startsWith("\\u")) {
              UnicodeCharRegex.findPrefixOf(json) match {
                case Some(unicodeContent) => {
                  val unicodeCharToken = UnicodeCharacterToken(unicodeContent)
                  tokenize(unicodeCharToken.some, json.drop(unicodeContent.length), current :+ unicodeCharToken)
                }
                case _ => unexpectedContent(json)
              }
            } else {
              EscapedCharRegex.findPrefixOf(json) match {
                case Some(escapedChar) => {
                  val escapedCharToken = EscapedCharacterToken(escapedChar)
                  tokenize(escapedCharToken.some, json.drop(escapedChar.length), current :+ escapedCharToken)
                }
                case _ => unexpectedContent(json)
              }
           }
          } else {
            NormalStringRegex.findPrefixOf(json) match {
              case Some(normalStringContent) => {
                val normalStringToken = NormalStringToken(normalStringContent)
                tokenize(normalStringToken.some, json.drop(normalStringContent.length), current :+ normalStringToken)
              }
              case _ => unexpectedContent(json)
            }
          }
        }
        case _ => {
          val jsonHead = json.head
          jsonHead match {
            case '[' => tokenize(ArrayOpenToken.some, json.tail, current :+ ArrayOpenToken)
            case ']' => tokenize(ArrayCloseToken.some, json.tail, current :+ ArrayCloseToken)
            case '{' => tokenize(ObjectOpenToken.some, json.tail, current :+ ObjectOpenToken)
            case '}' => tokenize(ObjectCloseToken.some, json.tail, current :+ ObjectCloseToken)
            case ':' => tokenize(FieldSeparatorToken.some, json.tail, current :+ FieldSeparatorToken)
            case ',' => tokenize(EntrySeparatorToken.some, json.tail, current :+ EntrySeparatorToken)
            case '"' => tokenize(StringBoundsOpenToken.some, json.tail, current :+ StringBoundsOpenToken)
            case ' ' => tokenize(previousToken, json.tail, current)
            case '\r' => tokenize(previousToken, json.tail, current)
            case '\n' => tokenize(previousToken, json.tail, current)
            case _ => {
              json match {
                case trueStartingJSON if trueStartingJSON.startsWith("true") => tokenize(BooleanTrueToken.some, json.drop(4), current :+ BooleanTrueToken)
                case falseStartingJSON if falseStartingJSON.startsWith("false") => tokenize(BooleanFalseToken.some, json.drop(5), current :+ BooleanFalseToken)
                case nullStartingJSON if nullStartingJSON.startsWith("null") => tokenize(NullToken.some, json.drop(4), current :+ NullToken)
                case _ => {
                  NumberRegex.findPrefixOf(json) match {
                    case Some(numberString) => {
                      val numberToken = NumberToken(numberString)
                      tokenize(numberToken.some, json.drop(numberString.length), current :+ numberToken)
                    }
                    case _ => unexpectedContent(json)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
