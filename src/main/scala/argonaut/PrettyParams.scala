package argonaut

import scalaz._, Scalaz._
import scala.annotation._
import monocle.Macro._

/**
 * Parameters for pretty-printing a JSON value.
 *
 * @author Tony Morris
 *
 * @param lbraceLeft Takes the current depth and returns the spaces to insert to left of a left brace.
 * @param lbraceRight Takes the current depth and returns the spaces to insert to right of a left brace.
 * @param rbraceLeft Takes the current depth and returns the spaces to insert to left of a right brace.
 * @param rbraceRight Takes the current depth and returns the spaces to insert to right of a right brace.
 * @param lbracketLeft Takes the current depth and returns the spaces to insert to left of a left bracket.
 * @param lbracketRight Takes the current depth and returns the spaces to insert to right of a left bracket.
 * @param rbracketLeft Takes the current depth and returns the spaces to insert to left of a right bracket.
 * @param rbracketRight Takes the current depth and returns the spaces to insert to right of a right bracket.
 * @param commaLeft Takes the current depth and returns the spaces to insert to left of a comma.
 * @param commaRight Takes the current depth and returns the spaces to insert to right of a comma.
 * @param colonLeft Takes the current depth and returns the spaces to insert to left of a colon.
 * @param colonRight Takes the current depth and returns the spaces to insert to right of a colon.
 * @param preserveOrder Determines if the field ordering should be preserved.
 */
case class PrettyParams(
    lbraceLeft: Int => String
  , lbraceRight: Int => String
  , rbraceLeft: Int => String
  , rbraceRight: Int => String
  , lbracketLeft: Int => String
  , lbracketRight: Int => String
  , rbracketLeft: Int => String
  , rbracketRight: Int => String
  , commaLeft: Int => String
  , commaRight: Int => String
  , colonLeft: Int => String
  , colonRight: Int => String
  , preserveOrder: Boolean
) {

  private[this] final val openBraceText = "{"
  private[this] final val closeBraceText = "}"
  private[this] final val openArrayText = "["
  private[this] final val closeArrayText = "]"
  private[this] final val commaText = ","
  private[this] final val colonText = ":"
  private[this] final val nullText = "null"
  private[this] final val trueText = "true"
  private[this] final val falseText = "false"
  private[this] final val stringEnclosureText = "\""

  import Memo._

  private[this] def vectorMemo() = {
    var vector: Vector[String] = Vector.empty

    val memoFunction: (Int => String) => Int => String = f => k => {
      val localVector = vector
      val adjustedK = if (k < 0) 0 else k
      if (localVector.size > adjustedK) {
        localVector(adjustedK)
      } else {
        val newVector = Vector.tabulate(adjustedK + 1)(f)
        vector = newVector
        newVector.last
      }
    }
    memo(memoFunction)
  }

  // TODO: Vector based memoisation.
  private[this] final val lbraceMemo = vectorMemo(){depth: Int => "%s%s%s".format(lbraceLeft(depth), openBraceText, lbraceRight(depth + 1))}
  private[this] final val rbraceMemo = vectorMemo(){depth: Int => "%s%s%s".format(rbraceLeft(depth + 1), closeBraceText, rbraceRight(depth + 1))}
  private[this] final val lbracketMemo = vectorMemo(){depth: Int => "%s%s%s".format(lbracketLeft(depth), openArrayText, lbracketRight(depth + 1))}
  private[this] final val rbracketMemo = vectorMemo(){depth: Int => "%s%s%s".format(rbracketLeft(depth + 1), closeArrayText, rbracketRight(depth))}
  private[this] final val commaMemo = vectorMemo(){depth: Int => "%s%s%s".format(commaLeft(depth + 1), commaText, commaRight(depth + 1))}
  private[this] final val colonMemo = vectorMemo(){depth: Int => "%s%s%s".format(colonLeft(depth + 1), colonText, colonRight(depth + 1))}

  /**
   * Returns a string representation of a pretty-printed JSON value.
   */
  final def pretty(j: Json): String = {
    import Json._
    import StringEscaping._

    @tailrec
    def appendJsonString(builder: StringBuilder, jsonString: String, normalChars: Boolean = true): StringBuilder = {
      if (normalChars) {
        jsonString.span(isNormalChar) match {
          case (prefix, suffix) => {
            val prefixAppended = builder.append(prefix)
            if (suffix.isEmpty) prefixAppended else appendJsonString(prefixAppended, suffix, false)
          }
        }
      } else {
        jsonString.span(isNotNormalChar) match {
          case (prefix, suffix) => {
            val prefixAppended = prefix.foldLeft(builder)((working, char) => working.append(escape(char)))
            if (suffix.isEmpty) prefixAppended else appendJsonString(prefixAppended, suffix, true)
          }
        }
      }
    }

    def encloseJsonString(builder: StringBuilder, jsonString: JsonString): StringBuilder = {
      appendJsonString(builder.append(stringEnclosureText), jsonString).append(stringEnclosureText)
    }

    def trav(builder: StringBuilder, depth: Int, k: Json): StringBuilder = {

      def lbrace(builder: StringBuilder): StringBuilder = {
        builder.append(lbraceMemo(depth))
      }
      def rbrace(builder: StringBuilder): StringBuilder = {
        builder.append(rbraceMemo(depth))
      }
      def lbracket(builder: StringBuilder): StringBuilder = {
        builder.append(lbracketMemo(depth))
      }
      def rbracket(builder: StringBuilder): StringBuilder = {
        builder.append(rbracketMemo(depth))
      }
      def comma(builder: StringBuilder): StringBuilder = {
        builder.append(commaMemo(depth))
      }
      def colon(builder: StringBuilder): StringBuilder = {
        builder.append(colonMemo(depth))
      }

      k.fold[StringBuilder](
        builder.append(nullText)
        , bool => builder.append(if (bool) trueText else falseText)
        , n => n match {
          case JsonLong(x) => builder append x.toString
          case JsonDouble(x) => builder append x.toString
          case JsonDecimal(x) => builder append x
          case JsonBigDecimal(x) => builder append x.toString
        }
        , s => encloseJsonString(builder, s)
        , e => {
          rbracket(e.foldLeft((true, lbracket(builder))){case ((firstElement, builder), subElement) =>
            val withComma = if(firstElement) builder else comma(builder)
            val updatedBuilder = trav(withComma, depth + 1, subElement)
            (false, updatedBuilder)
          }._2)
        }
        , o => {
          rbrace((if (preserveOrder) o.toList else o.toMap).foldLeft((true, lbrace(builder))){case ((firstElement, builder), (key, value)) =>
            val withComma = if(firstElement) builder else comma(builder)
            val updatedBuilder = trav(colon(encloseJsonString(withComma, key)), depth + 1, value)
            (false, updatedBuilder)
          }._2)
        }
      )
    }

    trav(new StringBuilder(), 0, j).toString()
  }

  /**
   * Returns a `Vector[Char]` representation of a pretty-printed JSON value.
   */
  final def lpretty(j: Json): Vector[Char] = Vector.empty[Char] ++ pretty(j)
}

object StringEscaping {
  final def escape(c: Char): String = (c: @switch) match {
    case '\\' => "\\\\"
    case '"' => "\\\""
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case possibleUnicode => if (Character.isISOControl(possibleUnicode)) "\\u%04x".format(possibleUnicode.toInt) else possibleUnicode.toString
  }
  final val isNormalChar: Char => Boolean = char => (char: @switch) match {
    case '\\' => false
    case '"' => false
    case '\b' => false
    case '\f' => false
    case '\n' => false
    case '\r' => false
    case '\t' => false
    case possibleUnicode => !Character.isISOControl(possibleUnicode)
  }
  final val isNotNormalChar: Char => Boolean = char => !isNormalChar(char)
}

object PrettyParams extends PrettyParamss

trait PrettyParamss {
  private[this] final val zeroString = (_: Int) => ""
  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  final val nospace: PrettyParams =
    PrettyParams(
      lbraceLeft = zeroString
    , lbraceRight = zeroString
    , rbraceLeft = zeroString
    , rbraceRight = zeroString
    , lbracketLeft = zeroString
    , lbracketRight = zeroString
    , rbracketLeft = zeroString
    , rbracketRight = zeroString
    , commaLeft = zeroString
    , commaRight = zeroString
    , colonLeft = zeroString
    , colonRight = zeroString
    , preserveOrder = false
    )

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  final def pretty(indent: String): PrettyParams =
    PrettyParams(
      lbraceLeft = zeroString
    , lbraceRight = n => "\n" + indent * n
    , rbraceLeft = n => "\n" + indent * (n - 1)
    , rbraceRight = zeroString
    , lbracketLeft = zeroString
    , lbracketRight = n => "\n" + indent * n
    , rbracketLeft = n => "\n" + indent * (n - 1)
    , rbracketRight = zeroString
    , commaLeft = zeroString
    , commaRight = n => "\n" + indent * n
    , colonLeft = n => " "
    , colonRight = n => " "
    , preserveOrder = false
    )

  /**
   * A pretty-printer configuration that indents by two spaces.
   */
  final val spaces2: PrettyParams =
    pretty("  ")

  /**
   * A pretty-printer configuration that indents by four spaces.
   */
  final val spaces4: PrettyParams =
    pretty("    ")

  val lbraceLeftL = mkLens[PrettyParams, Int => String]("lbraceLeft")
  val lbraceRightL = mkLens[PrettyParams, Int => String]("lbraceRight")
  val rbraceLeftL = mkLens[PrettyParams, Int => String]("rbraceLeft")
  val rbraceRightL = mkLens[PrettyParams, Int => String]("rbraceRight")
  val lbracketLeftL = mkLens[PrettyParams, Int => String]("lbracketLeft")
  val lbracketRightL = mkLens[PrettyParams, Int => String]("lbracketRight")
  val rbracketLeftL = mkLens[PrettyParams, Int => String]("rbracketLeft")
  val rbracketRightL = mkLens[PrettyParams, Int => String]("rbracketRight")
  val commaLeftL = mkLens[PrettyParams, Int => String]("commaLeft")
  val commaRightL = mkLens[PrettyParams, Int => String]("commaRight")
  val colonLeftL = mkLens[PrettyParams, Int => String]("colonLeft")
  val colonRightL = mkLens[PrettyParams, Int => String]("colonRight")
  val preserveOrderL = mkLens[PrettyParams, Boolean]("preserveOrder")
}
