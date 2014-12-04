package argonaut

import scalaz._, Scalaz._
import scala.annotation._

/**
 * Parameters for pretty-printing a JSON value.
 *
 * @author Tony Morris
 */
sealed trait PrettyParams {
  /**
   * Takes the current depth and returns the spaces to insert to left of a left brace.
   */
  val lbraceLeft: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to right of a left brace.
   */
  val lbraceRight: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to left of a right brace.
   */
  val rbraceLeft: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to right of a right brace.
   */
  val rbraceRight: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to left of a left bracket.
   */
  val lbracketLeft: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to right of a left bracket.
   */
  val lbracketRight: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to left of a right bracket.
   */
  val rbracketLeft: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to right of a right bracket.
   */
  val rbracketRight: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to left of a comma.
   */
  val commaLeft: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to right of a comma.
   */
  val commaRight: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to left of a colon.
   */
  val colonLeft: Int => String

  /**
   * Takes the current depth and returns the spaces to insert to right of a colon.
   */
  val colonRight: Int => String

  /**
   * Determines if the field ordering should be preserved.
   */
  val preserveOrder: Boolean

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

object PrettyParams extends PrettyParamss {
  def apply(
             lbraceLeft0: Int => String
           , lbraceRight0: Int => String
           , rbraceLeft0: Int => String
           , rbraceRight0: Int => String
           , lbracketLeft0: Int => String
           , lbracketRight0: Int => String
           , rbracketLeft0: Int => String
           , rbracketRight0: Int => String
           , commaLeft0: Int => String
           , commaRight0: Int => String
           , colonLeft0: Int => String
           , colonRight0: Int => String
           , preserveOrder0: Boolean
           ): PrettyParams =
    new PrettyParams {
      val lbraceLeft = lbraceLeft0
      val lbraceRight = lbraceRight0
      val rbraceLeft = rbraceLeft0
      val rbraceRight = rbraceRight0
      val lbracketLeft = lbracketLeft0
      val lbracketRight = lbracketRight0
      val rbracketLeft = rbracketLeft0
      val rbracketRight = rbracketRight0
      val commaLeft = commaLeft0
      val commaRight = commaRight0
      val colonLeft = colonLeft0
      val colonRight = colonRight0
      val preserveOrder = preserveOrder0
    }
}

trait PrettyParamss {
  private[this] final val zeroString = (_: Int) => ""
  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  final val nospace: PrettyParams =
    PrettyParams(
      zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , zeroString
    , false
    )

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  final def pretty(indent: String): PrettyParams =
    PrettyParams(
      zeroString
    , n => "\n" + indent * n
    , n => "\n" + indent * (n - 1)
    , zeroString
    , zeroString
    , n => "\n" + indent * n
    , n => "\n" + indent * (n - 1)
    , zeroString
    , zeroString
    , n => "\n" + indent * n
    , n => " "
    , n => " "
    , false
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

  /**
   * The lens to the `lbraceLeft` configuration value.
   */
  def lbraceLeftL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(_, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.lbraceLeft))

  /**
   * The lens to the `lbraceRight` configuration value.
   */
  def lbraceRightL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, _, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.lbraceRight))

  /**
   * The lens to the `rbraceLeft` configuration value.
   */
  def rbraceLeftL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, _, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.rbraceLeft))

  /**
   * The lens to the `rbraceRight` configuration value.
   */
  def rbraceRightL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, _, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.rbraceRight))

  /**
   * The lens to the `lbracketLeft` configuration value.
   */
  def lbracketLeftL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, _, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.lbracketLeft))

  /**
   * The lens to the `lbracketRight` configuration value.
   */
  def lbracketRightL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, _, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.lbracketRight))

  /**
   * The lens to the `rbracketLeft` configuration value.
   */
  def rbracketLeftL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, _, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.rbracketLeft))

  /**
   * The lens to the `rbracketRight` configuration value.
   */
  def rbracketRightL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, _, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.rbracketRight))

  /**
   * The lens to the `commaLeft` configuration value.
   */
  def commaLeftL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, _, p.commaRight, p.colonLeft, p.colonRight, p.preserveOrder), p.commaLeft))

  /**
   * The lens to the `commaRight` configuration value.
   */
  def commaRightL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, _, p.colonLeft, p.colonRight, p.preserveOrder), p.commaRight))

  /**
   * The lens to the `colonLeft` configuration value.
   */
  def colonLeftL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, _, p.colonRight, p.preserveOrder), p.colonLeft))

  /**
   * The lens to the `colonRight` configuration value.
   */
  def colonRightL: PrettyParams @> (Int => String) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, _, p.preserveOrder), p.colonRight))

  /**
   * The lens to the `preserveOrder` configuration value.
   */
  def preserveOrderL: PrettyParams @> Boolean =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, _), p.preserveOrder))
}
