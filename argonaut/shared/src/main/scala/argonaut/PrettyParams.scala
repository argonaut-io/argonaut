package argonaut

import scala.annotation._

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
 * @param lrbracketsEmpty Spaces to insert for an empty array.
 * @param arrayCommaLeft Spaces to insert to left of a comma in an array.
 * @param arrayCommaRight Spaces to insert to right of a comma in an array.
 * @param objectCommaLeft Spaces to insert to left of a comma in an object.
 * @param objectCommaRight Spaces to insert to right of a comma in an object.
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
  , lrbracketsEmpty: String
  , arrayCommaLeft: String
  , arrayCommaRight: String
  , objectCommaLeft: String
  , objectCommaRight: String
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
  private[this] val _lrbracketsEmpty = addIndentation(lrbracketsEmpty)
  private[this] val _arrayCommaLeft = addIndentation(arrayCommaLeft)
  private[this] val _arrayCommaRight = addIndentation(arrayCommaRight)
  private[this] val _objectCommaLeft = addIndentation(objectCommaLeft)
  private[this] val _objectCommaRight = addIndentation(objectCommaRight)
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

  // TODO: Vector based memoisation.
  private[this] final val lbraceMemo = PrettyParams.vectorMemo{depth: Int => _lbraceLeft(depth) + openBraceText + _lbraceRight(depth + 1)}
  private[this] final val rbraceMemo = PrettyParams.vectorMemo{depth: Int => _rbraceLeft(depth) + closeBraceText + _rbraceRight(depth + 1)}
  private[this] final val lbracketMemo = PrettyParams.vectorMemo{depth: Int => _lbracketLeft(depth) + openArrayText + _lbracketRight(depth + 1)}
  private[this] final val rbracketMemo = PrettyParams.vectorMemo{depth: Int => _rbracketLeft(depth) + closeArrayText + _rbracketRight(depth)}
  private[this] final val lrbracketsEmptyMemo = PrettyParams.vectorMemo{depth: Int => openArrayText + _lrbracketsEmpty(depth) + closeArrayText}
  private[this] final val arrayCommaMemo = PrettyParams.vectorMemo{depth: Int => _arrayCommaLeft(depth + 1) + commaText + _arrayCommaRight(depth + 1)}
  private[this] final val objectCommaMemo = PrettyParams.vectorMemo{depth: Int => _objectCommaLeft(depth + 1) + commaText + _objectCommaRight(depth + 1)}
  private[this] final val colonMemo = PrettyParams.vectorMemo{depth: Int => _colonLeft(depth + 1) + colonText + _colonRight(depth + 1)}

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
      def lrbracketsEmpty(builder: StringBuilder): StringBuilder = {
        builder.append(lrbracketsEmptyMemo(depth))
      }
      def arrayComma(builder: StringBuilder): StringBuilder = {
        builder.append(arrayCommaMemo(depth))
      }
      def objectComma(builder: StringBuilder): StringBuilder = {
        builder.append(objectCommaMemo(depth))
      }
      def colon(builder: StringBuilder): StringBuilder = {
        builder.append(colonMemo(depth))
      }

      k.fold[StringBuilder](
        builder.append(nullText)
        , bool => builder.append(if (bool) trueText else falseText)
        , n => n match {
          case JsonLong(x) => builder.append(x.toString)
          case JsonDecimal(x) => builder.append(x)
          case JsonBigDecimal(x) => builder.append(x.toString)
        }
        , s => encloseJsonString(builder, s)
        , e => if (e.isEmpty) {
          lrbracketsEmpty(builder)
        } else {
          rbracket(e.foldLeft((true, lbracket(builder))){case ((firstElement, builder), subElement) =>
            val withComma = if (firstElement) builder else arrayComma(builder)
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
              val withComma = if (firstElement) builder else objectComma(builder)
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

object PrettyParams extends PrettyParamss {
  def vectorMemo(f: Int => String): Int => String = {
    var vector: Vector[String] = Vector.empty

    (i: Int) => {
      if (i >= 0) {
        val captured = vector
        if (captured.size <= i) {
          val tabulated = Vector.tabulate(i + 1)(f)
          val result = tabulated(i)
          if (vector.size < tabulated.size) {
            vector = tabulated
          }
          result
        } else {
          captured(i)
        }
      } else {
        ""
      }
    }
  }
}

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
    , lrbracketsEmpty = ""
    , arrayCommaLeft = ""
    , arrayCommaRight = ""
    , objectCommaLeft = ""
    , objectCommaRight = ""
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
    , lrbracketsEmpty = ""
    , arrayCommaLeft = ""
    , arrayCommaRight = "\n"
    , objectCommaLeft = ""
    , objectCommaRight = "\n"
    , colonLeft = " "
    , colonRight = " "
    , preserveOrder = false
    , dropNullKeys = false
    )

  /**
   * A pretty-printer configuration that indents by two spaces.
   */
  final val spaces2: PrettyParams = pretty("  ")

  /**
   * A pretty-printer configuration that indents by four spaces.
   */
  final val spaces4: PrettyParams = pretty("    ")
}
