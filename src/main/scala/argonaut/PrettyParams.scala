package argonaut

import scalaz._, Scalaz._
import scala.annotation._
import monocle.Macro._

/**
 * Parameters for pretty-printing a JSON value.
 *
 * @author Tony Morris
 *
 * @param indent The indentation to use if any format strings contain a new line.
 * @param lbraceLeft Spaces to insert to left of a left brace.
 * @param lbraceRight Spaces to insert to right of a left brace.
 * @param rbraceLeft Spaces to insert to left of a right brace.
 * @param rbraceRight Spaces to insert to right of a right brace.
 * @param lbracketLeft Spaces to insert to left of a left bracket.
 * @param lbracketRight Spaces to insert to right of a left bracket.
 * @param rbracketLeft Spaces to insert to left of a right bracket.
 * @param rbracketRight Spaces to insert to right of a right bracket.
 * @param commaLeft Spaces to insert to left of a comma.
 * @param commaRight Spaces to insert to right of a comma.
 * @param colonLeft Spaces to insert to left of a colon.
 * @param colonRight Spaces to insert to right of a colon.
 * @param preserveOrder Determines if field ordering should be preserved.
 * @param dropNullKeys Determines if object fields with values of null are dropped from the output.
 */
case class PrettyParams(
    indent: String
  , lbraceLeft: String
  , lbraceRight: String
  , rbraceLeft: String
  , rbraceRight: String
  , lbracketLeft: String
  , lbracketRight: String
  , rbracketLeft: String
  , rbracketRight: String
  , commaLeft: String
  , commaRight: String
  , colonLeft: String
  , colonRight: String
  , preserveOrder: Boolean
  , dropNullKeys: Boolean
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

  private[this] val _lbraceLeft = addIndentation(lbraceLeft)
  private[this] val _lbraceRight = addIndentation(lbraceRight)
  private[this] val _rbraceLeft = addIndentation(rbraceLeft)
  private[this] val _rbraceRight = addIndentation(rbraceRight)
  private[this] val _lbracketLeft = addIndentation(lbracketLeft)
  private[this] val _lbracketRight = addIndentation(lbracketRight)
  private[this] val _rbracketLeft = addIndentation(rbracketLeft)
  private[this] val _rbracketRight = addIndentation(rbracketRight)
  private[this] val _commaLeft = addIndentation(commaLeft)
  private[this] val _commaRight = addIndentation(commaRight)
  private[this] val _colonLeft = addIndentation(colonLeft)
  private[this] val _colonRight = addIndentation(colonRight)

  private[this] def addIndentation(s: String): Int => String = {
    val lastNewLineIndex = s.lastIndexOf("\n")
    if (lastNewLineIndex < 0) {
      _ => s
    } else {
      val afterLastNewLineIndex = lastNewLineIndex + 1
      val start = s.substring(0, afterLastNewLineIndex)
      val end = s.substring(afterLastNewLineIndex)
      n => start + indent * n + end
    }
  }

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
  private[this] final val lbraceMemo = vectorMemo(){depth: Int => "%s%s%s".format(_lbraceLeft(depth), openBraceText, _lbraceRight(depth + 1))}
  private[this] final val rbraceMemo = vectorMemo(){depth: Int => "%s%s%s".format(_rbraceLeft(depth), closeBraceText, _rbraceRight(depth + 1))}
  private[this] final val lbracketMemo = vectorMemo(){depth: Int => "%s%s%s".format(_lbracketLeft(depth), openArrayText, _lbracketRight(depth + 1))}
  private[this] final val rbracketMemo = vectorMemo(){depth: Int => "%s%s%s".format(_rbracketLeft(depth), closeArrayText, _rbracketRight(depth))}
  private[this] final val commaMemo = vectorMemo(){depth: Int => "%s%s%s".format(_commaLeft(depth + 1), commaText, _commaRight(depth + 1))}
  private[this] final val colonMemo = vectorMemo(){depth: Int => "%s%s%s".format(_colonLeft(depth + 1), colonText, _colonRight(depth + 1))}

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
        , n => builder.append(if (n == n.floor) BigDecimal(n).toBigInt.toString else n.toString)
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
            val ignoreKey = dropNullKeys && value.isNull
            if (ignoreKey) {
              (firstElement, builder)
            } else {
              val withComma = if (firstElement) builder else comma(builder)
              (false, trav(colon(encloseJsonString(withComma, key)), depth + 1, value))
            }
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

  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  final val nospace: PrettyParams =
    PrettyParams(
      indent = ""
    , lbraceLeft = ""
    , lbraceRight = ""
    , rbraceLeft = ""
    , rbraceRight = ""
    , lbracketLeft = ""
    , lbracketRight = ""
    , rbracketLeft = ""
    , rbracketRight = ""
    , commaLeft = ""
    , commaRight = ""
    , colonLeft = ""
    , colonRight = ""
    , preserveOrder = false
    , dropNullKeys = false
    )

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  final def pretty(indent: String): PrettyParams =
    PrettyParams(
      indent = indent
    , lbraceLeft = ""
    , lbraceRight = "\n"
    , rbraceLeft = "\n"
    , rbraceRight = ""
    , lbracketLeft = ""
    , lbracketRight = "\n"
    , rbracketLeft = "\n"
    , rbracketRight = ""
    , commaLeft = ""
    , commaRight = "\n"
    , colonLeft = " "
    , colonRight = " "
    , preserveOrder = false
    , dropNullKeys = false
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

  val indentL = mkLens[PrettyParams, String]("indent")
  val lbraceLeftL = mkLens[PrettyParams, String]("lbraceLeft")
  val lbraceRightL = mkLens[PrettyParams, String]("lbraceRight")
  val rbraceLeftL = mkLens[PrettyParams, String]("rbraceLeft")
  val rbraceRightL = mkLens[PrettyParams, String]("rbraceRight")
  val lbracketLeftL = mkLens[PrettyParams, String]("lbracketLeft")
  val lbracketRightL = mkLens[PrettyParams, String]("lbracketRight")
  val rbracketLeftL = mkLens[PrettyParams, String]("rbracketLeft")
  val rbracketRightL = mkLens[PrettyParams, String]("rbracketRight")
  val commaLeftL = mkLens[PrettyParams, String]("commaLeft")
  val commaRightL = mkLens[PrettyParams, String]("commaRight")
  val colonLeftL = mkLens[PrettyParams, String]("colonLeft")
  val colonRightL = mkLens[PrettyParams, String]("colonRight")
  val preserveOrderL = mkLens[PrettyParams, Boolean]("preserveOrder")
  val dropNullKeysL = mkLens[PrettyParams, Boolean]("dropNullKeys")
}
