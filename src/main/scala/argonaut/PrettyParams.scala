package argonaut

import scalaz._, Scalaz._

/**
 * Parameters for pretty-printing a JSON value.
 *
 * @author Tony Morris
 */
sealed trait PrettyParams {
  /**
   * Takes the current depth and returns the spaces to insert to left of a left brace.
   */
  val lbraceLeft: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to right of a left brace.
   */
  val lbraceRight: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to left of a right brace.
   */
  val rbraceLeft: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to right of a right brace.
   */
  val rbraceRight: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to left of a left bracket.
   */
  val lbracketLeft: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to right of a left bracket.
   */
  val lbracketRight: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to left of a right bracket.
   */
  val rbracketLeft: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to right of a right bracket.
   */
  val rbracketRight: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to left of a comma.
   */
  val commaLeft: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to right of a comma.
   */
  val commaRight: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to left of a colon.
   */
  val colonLeft: Int => JsonWhitespaces

  /**
   * Takes the current depth and returns the spaces to insert to right of a colon.
   */
  val colonRight: Int => JsonWhitespaces

  /**
   * Returns a string representation of a pretty-printed JSON value.
   */
  def pretty(j: Json): String = {
    import Json._
    import StringEscaping._

    def appendJsonString(jsonString: JsonString, stringBuilder: StringBuilder): StringBuilder = {
      jsonString
        .foldLeft(stringBuilder.append(stringEnclosureText))((builder, string) => builder.append(escape(string)))
        .append(stringEnclosureText)
    }
    def trav(depth: Int, k: Json, stringBuilder: StringBuilder): StringBuilder = {
      def lbraceAppend(stringBuilder: StringBuilder): StringBuilder = {
         lbraceLeft(depth).append(stringBuilder).append(openBraceText) |> lbraceRight(depth + 1).append
      }
      def rbraceAppend(stringBuilder: StringBuilder): StringBuilder = {
        rbraceLeft(depth + 1).append(stringBuilder).append(closeBraceText) |> rbraceRight(depth).append
      }
      def lbracketAppend(stringBuilder: StringBuilder): StringBuilder = {
        lbracketLeft(depth).append(stringBuilder).append(openArrayText) |> lbracketRight(depth + 1).append
      }
      def rbracketAppend(stringBuilder: StringBuilder): StringBuilder = {
        rbracketLeft(depth + 1).append(stringBuilder).append(closeArrayText) |> rbracketRight(depth).append
      }
      def commaAppend(stringBuilder: StringBuilder): StringBuilder = {
        commaLeft(depth + 1).append(stringBuilder).append(commaText) |>commaRight(depth + 1).append
      }
      def colonAppend(stringBuilder: StringBuilder): StringBuilder = {
        colonLeft(depth + 1).append(stringBuilder).append(colonText) |> colonRight(depth + 1).append
      }

      k.fold(
        stringBuilder.append(nullText)
        , bool => stringBuilder.append(bool ? trueText | falseText)
        , n => stringBuilder.append(n.shows)
        , s => appendJsonString(s, stringBuilder)
        , e => {
          val arrayAppends: List[StringBuilder => StringBuilder] = e.toList.map{elem => (builder: StringBuilder) =>
            trav(depth + 1, elem, builder)
          }
          arrayAppends.intersperse(commaAppend).foldLeft(lbracketAppend(stringBuilder))((builder, elem) => elem(builder)) |> rbracketAppend
        }
        , o => {
          val associationAppends: List[StringBuilder => StringBuilder] = o.toList.map{assoc => (builder: StringBuilder) =>
            val innerAppend = (b: StringBuilder) => trav(depth + 1, assoc._2, b)
            appendJsonString(assoc._1, builder) |> colonAppend |> innerAppend
          }
          associationAppends.intersperse(commaAppend).foldLeft(lbraceAppend(stringBuilder))((builder, elem) => elem(builder)) |> rbraceAppend
        }
      )
    }

    trav(0, j, new StringBuilder()).toString()
  }

  private[this] final val openBraceText = '{'
  private[this] final val closeBraceText = '}'
  private[this] final val openArrayText = '['
  private[this] final val closeArrayText = ']'
  private[this] final val commaText = ','
  private[this] final val colonText = ':'
  private[this] final val nullText = "null"
  private[this] final val trueText = "true"
  private[this] final val falseText = "false"
  private[this] final val stringEnclosureText = "\""

  /**
   * Returns a `Vector[Char]` representation of a pretty-printed JSON value.
   */
  def lpretty(j: Json): Vector[Char] = Vector.empty[Char] ++ pretty(j)
}

object StringEscaping {
  def escape(c: Char): String =
    c match {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case possibleUnicode if possibleUnicode.isControl => "\\u%04x".format(possibleUnicode.toInt)
      case _ => c.toString
    }
}

object PrettyParams extends PrettyParamss {
  def apply(
             lbraceLeft0: Int => JsonWhitespaces
           , lbraceRight0: Int => JsonWhitespaces
           , rbraceLeft0: Int => JsonWhitespaces
           , rbraceRight0: Int => JsonWhitespaces
           , lbracketLeft0: Int => JsonWhitespaces
           , lbracketRight0: Int => JsonWhitespaces
           , rbracketLeft0: Int => JsonWhitespaces
           , rbracketRight0: Int => JsonWhitespaces
           , commaLeft0: Int => JsonWhitespaces
           , commaRight0: Int => JsonWhitespaces
           , colonLeft0: Int => JsonWhitespaces
           , colonRight0: Int => JsonWhitespaces
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
    }
}

trait PrettyParamss {
  private[this] final val whitespacesZero = Monoid[JsonWhitespaces].zero
  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  def nospace: PrettyParams =
    PrettyParams(
      _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    , _ => whitespacesZero
    )

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  def pretty(indent: JsonWhitespaces): PrettyParams =
    PrettyParams(
      _ => whitespacesZero
    , n => JsonLine +: indent * n
    , n => JsonLine +: indent * (n - 1)
    , _ => whitespacesZero
    , _ => whitespacesZero
    , n => JsonLine +: indent * n
    , n => JsonLine +: indent * (n - 1)
    , _ => whitespacesZero
    , _ => whitespacesZero
    , n => JsonLine +: indent * n
    , _ => +JsonSpace
    , _ => +JsonSpace
    )

  /**
   * A pretty-printer configuration that indents by two spaces.
   */
  def spaces2: PrettyParams =
    pretty(JsonSpace * 2)

  /**
   * A pretty-printer configuration that indents by four spaces.
   */
  def spaces4: PrettyParams =
    pretty(JsonSpace * 4)
}

/**
 * Represents a whitespace character that are permitted in a JSON string.
 *
 * @author Tony Morris
 */
sealed trait JsonWhitespace {
  /**
   * Converts this white-space value to a character.
   */
  def toChar: Char =
    this match {
      case JsonSpace => ' ' // %x20
      case JsonTab => '\t' // %x09
      case JsonLine => '\n' // %x0A
      case JsonReturn => '\r' // %x0D
    }

  /**
   * Reproduce this white-space value the given number of times.
   */
  def *(n: Int): JsonWhitespaces = {
    @annotation.tailrec
    def go(x: Int, w: JsonWhitespaces): JsonWhitespaces =
      if(x <= 0)
        w
      else
        go(x - 1, this +: w)
    go(n, Monoid[JsonWhitespaces].zero)
  }

  /**
   * Create a white-space string containing only this white-space value.
   */
  def unary_+ : JsonWhitespaces =
    JsonWhitespaces(this)
}

/**
 * A JSON space character.
 */
case object JsonSpace extends JsonWhitespace

/**
 * A JSON tab character.
 */
case object JsonTab extends JsonWhitespace

/**
 * A JSON newline character.
 */
case object JsonLine extends JsonWhitespace

/**
 * A JSON carriage-return character.
 */
case object JsonReturn extends JsonWhitespace

/**
 * A string of JSON white-space characters.
 *
 * @author Tony Morris
 */
sealed trait JsonWhitespaces {
  val value: Vector[JsonWhitespace]

  /**
   * Prepend the given white-space character.
   */
  def +:(s: JsonWhitespace): JsonWhitespaces =
    JsonWhitespaces.build(s +: value)

  /**
   * Append the given white-space character.
   */
  def :+(s: JsonWhitespace): JsonWhitespaces =
    JsonWhitespaces.build(value :+ s)

  /**
   * Append the given white-space characters.
   */
  def ++(s: JsonWhitespaces): JsonWhitespaces =
    JsonWhitespaces.build(value ++ s.value)

  /**
   * Reproduce this string of white-space characters the given number of times.
   */
  def *(n: Int): JsonWhitespaces = {
    @annotation.tailrec
    def go(x: Int, w: JsonWhitespaces): JsonWhitespaces =
      if(x <= 0)
        w
      else
        go(x - 1, this ++ w)
    go(n, Monoid[JsonWhitespaces].zero)
  }

  /**
   * Convert this string of white-space characters to a list.
   */
  def toList: List[JsonWhitespace] =
    value.toList

  /**
   * Convert this string of white-space characters to a string.
   */
  def string: String =
    value map (_.toChar) mkString

  /**
   * Convert this string of white-space characters to a `Vector[Char]`.
   */
  def chars: Vector[Char] =
    value map (_.toChar)

  /**
   * Appends this string of white-space characters to the given StringBuilder.
   */
  def append(stringBuilder: StringBuilder): StringBuilder = {
    value.foldLeft(stringBuilder)((builder, elem) => builder.append(elem.toChar))
  }
}

object JsonWhitespaces extends JsonWhitespacess {
  def apply(v: JsonWhitespace*): JsonWhitespaces =
    build(Vector(v: _*))

  private[argonaut] def build(v: Vector[JsonWhitespace]): JsonWhitespaces =
    new JsonWhitespaces {
      val value = v
    }
}

trait JsonWhitespacess {
  implicit val JsonWhitespacesInstances: Equal[JsonWhitespaces] with Show[JsonWhitespaces] with Monoid[JsonWhitespaces] =
    new Equal[JsonWhitespaces] with Show[JsonWhitespaces] with Monoid[JsonWhitespaces] {
      def equal(s1: JsonWhitespaces, s2: JsonWhitespaces) =
        s1.toList == s2.toList
      override def show(s: JsonWhitespaces) =
        s.toList map (_.toChar) mkString
      def zero =
        JsonWhitespaces.build(Vector())
      def append(s1: JsonWhitespaces, s2: => JsonWhitespaces) =
        s1 ++ s2
    }

  /**
   * The lens to the `lbraceLeft` configuration value.
   */
  def lbraceLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(_, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbraceLeft))

  /**
   * The lens to the `lbraceRight` configuration value.
   */
  def lbraceRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, _, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbraceRight))

  /**
   * The lens to the `rbraceLeft` configuration value.
   */
  def rbraceLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, _, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbraceLeft))

  /**
   * The lens to the `rbraceRight` configuration value.
   */
  def rbraceRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, _, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbraceRight))

  /**
   * The lens to the `lbracketLeft` configuration value.
   */
  def lbracketLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, _, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbracketLeft))

  /**
   * The lens to the `lbracketRight` configuration value.
   */
  def lbracketRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, _, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbracketRight))

  /**
   * The lens to the `rbracketLeft` configuration value.
   */
  def rbracketLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, _, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbracketLeft))

  /**
   * The lens to the `rbracketRight` configuration value.
   */
  def rbracketRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, _, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbracketRight))

  /**
   * The lens to the `commaLeft` configuration value.
   */
  def commaLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, _, p.commaRight, p.colonLeft, p.colonRight), p.commaLeft))

  /**
   * The lens to the `commaRight` configuration value.
   */
  def commaRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, _, p.colonLeft, p.colonRight), p.commaRight))

  /**
   * The lens to the `colonLeft` configuration value.
   */
  def colonLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, _, p.colonRight), p.colonLeft))

  /**
   * The lens to the `colonRight` configuration value.
   */
  def colonRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Store(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, _), p.colonRight))
}
