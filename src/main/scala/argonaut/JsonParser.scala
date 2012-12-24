package argonaut

import annotation.tailrec
import scalaz._
import Scalaz._

object JsonParser {
  sealed abstract class JSONToken {
    def originalStringContent: String
  }
  sealed abstract class OpenToken extends JSONToken
  sealed abstract class CloseToken extends JSONToken
  case object ArrayOpenToken extends OpenToken { 
    val originalStringContent = "["
    val inSome = this.some
  }
  case object ArrayCloseToken extends CloseToken { 
    val originalStringContent = "]"
    val inSome = this.some
  }
  case object ObjectOpenToken extends OpenToken { 
    val originalStringContent = "{" 
    val inSome = this.some
  }
  case object ObjectCloseToken extends CloseToken { 
    val originalStringContent = "}" 
    val inSome = this.some
  }
  case object EntrySeparatorToken extends JSONToken { 
    val originalStringContent = "," 
    val inSome = this.some
  }
  case object FieldSeparatorToken extends JSONToken { 
    val originalStringContent = ":" 
    val inSome = this.some
  }
  case object StringBoundsOpenToken extends OpenToken { 
    val originalStringContent = "\"" 
    val inSome = this.some
  }
  case object StringBoundsCloseToken extends CloseToken { 
    val originalStringContent = "\"" 
    val inSome = this.some
  }
  case class NumberToken(originalStringContent: String) extends JSONToken
  sealed abstract class BooleanToken extends JSONToken
  case object BooleanTrueToken extends BooleanToken { 
    val originalStringContent = "true" 
    val inSome = this.some
  }
  case object BooleanFalseToken extends BooleanToken { 
    val originalStringContent = "false" 
    val inSome = this.some
  }
  case object NullToken extends JSONToken { 
    val originalStringContent = "null" 
    val inSome = this.some
  }
  sealed abstract class StringPartToken extends JSONToken {
    def parsedStringContent: String
  }
  sealed case class UnicodeCharacterToken(unicodeSequence: String) extends StringPartToken {
    def originalStringContent = "\\u" + unicodeSequence
    def parsedStringContent = new java.lang.StringBuilder().appendCodePoint(Integer.valueOf(unicodeSequence, 16)).toString
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
    def parsedStringContent = originalStringContent
  }
  sealed case class UnexpectedContentToken(originalStringContent: String) extends JSONToken
  
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

  def tokenize(json: String): List[JSONToken] = tokenize(none, json).reverse

  private[this] final def expectedSpacerToken(stream: List[JSONToken], token: JSONToken, failMessage: String): ValidationNEL[String, List[JSONToken]] = {
    stream.headOption match {
      case Some(`token`) => stream.tail.successNel
      case _ => "%s but found: %s".format(failMessage, excerpt(stream)).failNel
    }
  }
  
  private[this] final def expectStringOpen(stream: List[JSONToken]) = expectedSpacerToken(stream, StringBoundsOpenToken, "Expected string bounds")

  private[this] final def expectStringClose(stream: List[JSONToken]) = expectedSpacerToken(stream, StringBoundsCloseToken, "Expected string bounds")

  private[this] final def expectArrayOpen(stream: List[JSONToken]) = expectedSpacerToken(stream, ArrayOpenToken, "Expected array open token")

  private[this] final def expectArrayClose(stream: List[JSONToken]) = expectedSpacerToken(stream, ArrayCloseToken, "Expected array close token")

  private[this] final def expectObjectOpen(stream: List[JSONToken]) = expectedSpacerToken(stream, ObjectOpenToken, "Expected object open token")

  private[this] final def expectObjectClose(stream: List[JSONToken]) = expectedSpacerToken(stream, ObjectCloseToken, "Expected object close token")

  private[this] final def expectEntrySeparator(stream: List[JSONToken]) = expectedSpacerToken(stream, EntrySeparatorToken, "Expected entry separator token")

  private[this] final def expectFieldSeparator(stream: List[JSONToken]) = expectedSpacerToken(stream, FieldSeparatorToken, "Expected field separator token")
  
  private[this] final def expectObject(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], JObject)] = {
    for {
      afterObjectOpen <- expectObjectOpen(stream)
      streamAndFields <- expectObjectField(true, (afterObjectOpen, List.empty).successNel)
      mappedVectorAndFields = streamAndFields.copy(_2 = streamAndFields._2.map(pair => (pair._1.s, pair._2)))
    } yield (streamAndFields._1, JObject(JsonObject(InsertionMap(mappedVectorAndFields._2.reverse: _*))))
  }
  
  private[this] final def expectArray(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], JArray)] = {
    for {
      afterArrayOpen <- expectArrayOpen(stream)
      streamAndFields <- expectArrayField(true, (afterArrayOpen, List.empty).successNel)
    } yield (streamAndFields._1, JArray(streamAndFields._2.reverse))
  }

  private[this] final def expectValue(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], Json)] = {
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

  @tailrec private[this] final def expectArrayField(first: Boolean, currentVector: ValidationNEL[String, (List[JSONToken], List[Json])]): ValidationNEL[String, (List[JSONToken], List[Json])] = {
    currentVector match {
      case Success((stream, fields)) => {
        stream.headOption match {
          case Some(ArrayCloseToken) => (stream.tail, fields).successNel
          case _ => {
            expectArrayField(false, for {
              afterEntrySeparator <- if (first) stream.successNel[String] else expectEntrySeparator(stream)
              streamAndValue <- expectValue(afterEntrySeparator)
            } yield (streamAndValue._1, streamAndValue._2 :: fields))
          }
        }
      }
      case _ => currentVector
    }
  }
  
  @tailrec private[this] final def expectObjectField(first: Boolean, currentVector: ValidationNEL[String, (List[JSONToken], List[(JString, Json)])]): ValidationNEL[String, (List[JSONToken], List[(JString, Json)])] = {
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
            } yield (streamAndValue._1, (streamAndKey._2, streamAndValue._2) :: fields))
          }
        }
      }
      case _ => currentVector
    }
  }

  private[this] final def expectString(stream: List[JSONToken]): ValidationNEL[String, (List[JSONToken], JString)] = {
    for {
      afterOpen <- expectStringOpen(stream)
      elements <- afterOpen.span(jsonToken => jsonToken.isInstanceOf[StringPartToken]).successNel[String]
      afterClose <- expectStringClose(elements._2)
    } yield (afterClose, JString(elements._1.collect{case stringPart: StringPartToken => stringPart.parsedStringContent}.mkString))
  }

  private[this] final def unexpectedContent(json: String) = List(UnexpectedContentToken(json.take(10)))

  private[this] final def parseNumber(json: String): Option[(NumberToken, String)] = {
    val (possibleNumber, remainder) = json.span(char => (char >= '0' && char <= '9') || char == '+' || char == '-' || char == 'e' || char == 'E' || char == '.')
    if (possibleNumber.isEmpty) None
    else (NumberToken(possibleNumber), remainder).some
  }
  
  @tailrec private[this] final def tokenize(previousToken: Option[JSONToken], json: String, current: List[JSONToken] = List.empty): List[JSONToken] = {
    if (json.isEmpty) current
    else {
      previousToken match {
        case Some(StringBoundsOpenToken) | Some(_: StringPartToken) => {
          if (json.head == '"') {
            tokenize(StringBoundsCloseToken.inSome, json.tail, StringBoundsCloseToken :: current)
          } else if (json.startsWith("""\""")) {
            if (json.startsWith("\\u")) {
              val possibleUnicodeSequence = json.drop(2).take(4)
              if (possibleUnicodeSequence.forall(char => (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F') || (char >= '0' && char <= '9'))) {
                val unicodeCharToken = UnicodeCharacterToken(possibleUnicodeSequence)
                tokenize(unicodeCharToken.some, json.drop(6), unicodeCharToken :: current)
              } else unexpectedContent(json)
            } else {
              EscapedCharacterToken.charMap.get(json.take(2)) match {
                case Some(escapedCharToken) => tokenize(escapedCharToken.some, json.drop(2), escapedCharToken :: current)
                case _ => unexpectedContent(json)
              }
            }
          } else {
            val (prefix: String, suffix: String) = json.span(char => !char.isControl && char != '"' && char != '\\')
            //println("json = " + json + ", prefix = " + prefix + ", suffix = " + suffix)
            val normalStringToken = NormalStringToken(prefix)
            suffix.headOption match {
              case Some('\"') | Some('\\') => tokenize(normalStringToken.some, suffix, normalStringToken :: current)
              case None => normalStringToken :: current
              case _ => {
                println("First char: " + (suffix.head.toLong))
                unexpectedContent(suffix)
              }
            }
          }
        }
        case _ => {
          val jsonHead = json.head
          jsonHead match {
            case '[' => tokenize(ArrayOpenToken.inSome, json.tail, ArrayOpenToken :: current)
            case ']' => tokenize(ArrayCloseToken.inSome, json.tail, ArrayCloseToken :: current)
            case '{' => tokenize(ObjectOpenToken.inSome, json.tail, ObjectOpenToken :: current)
            case '}' => tokenize(ObjectCloseToken.inSome, json.tail, ObjectCloseToken :: current)
            case ':' => tokenize(FieldSeparatorToken.inSome, json.tail, FieldSeparatorToken :: current)
            case ',' => tokenize(EntrySeparatorToken.inSome, json.tail, EntrySeparatorToken :: current)
            case '"' => tokenize(StringBoundsOpenToken.inSome, json.tail, StringBoundsOpenToken :: current)
            case ' ' => tokenize(previousToken, json.tail, current)
            case '\r' => tokenize(previousToken, json.tail, current)
            case '\n' => tokenize(previousToken, json.tail, current)
            case _ => {
              json match {
                case trueStartingJSON if trueStartingJSON.startsWith("true") => tokenize(BooleanTrueToken.inSome, json.drop(4), BooleanTrueToken :: current)
                case falseStartingJSON if falseStartingJSON.startsWith("false") => tokenize(BooleanFalseToken.inSome, json.drop(5), BooleanFalseToken :: current)
                case nullStartingJSON if nullStartingJSON.startsWith("null") => tokenize(NullToken.inSome, json.drop(4), NullToken :: current)
                case _ => {
                  parseNumber(json) match {
                    case Some((numberToken, remainder)) => tokenize(numberToken.some, remainder, numberToken :: current)
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
