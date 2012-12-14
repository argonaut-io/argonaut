package com.ephox
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
   * Takes the current depth and returns the spaces to insert to right of a right brace.
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
  def pretty(j: Json): String =
    lpretty(j).mkString

  private[this] final val openBraceVector = Vector('{')
  private[this] final val closeBraceVector = Vector('}')
  private[this] final val openArrayVector = Vector('[')
  private[this] final val closeArrayVector = Vector(']')
  private[this] final val commaVector = Vector(',')
  private[this] final val colonVector = Vector(':')
  private[this] final val nullVector = Vector('n', 'u', 'l', 'l')
  private[this] final val trueVector = Vector('t', 'r', 'u', 'e')
  private[this] final val falseVector = Vector('f', 'a', 'l', 's', 'e')

  /**
   * Returns a `Vector[Char]` representation of a pretty-printed JSON value.
   */
  def lpretty(j: Json): Vector[Char] = {
    def trav(depth: Int, k: Json): Vector[Char] = {
      lazy val lbrace = lbraceLeft(depth).chars ++ openBraceVector ++ lbraceRight(depth + 1).chars
      lazy val rbrace = rbraceLeft(depth + 1).chars ++ closeBraceVector ++ rbraceRight(depth).chars
      lazy val lbracket = lbracketLeft(depth).chars ++ openArrayVector ++ lbracketRight(depth + 1).chars
      lazy val rbracket = rbracketLeft(depth + 1).chars ++ closeArrayVector ++ rbracketRight(depth).chars
      lazy val comma = commaLeft(depth + 1).chars ++ commaVector ++ commaRight(depth + 1).chars
      lazy val colon = colonLeft(depth + 1).chars ++ colonVector ++ colonRight(depth + 1).chars

      import StringEscaping._
      k.fold(
        nullVector
      , if(_) trueVector else falseVector
      , n => Vector(Show[JsonNumber].show(n).toList: _*)
      , s => '"' +: Vector(s flatMap escape: _*) :+ '"'
      , e =>
          lbracket ++ e.reverse.foldLeft(false, Vector[Char]())({
            case ((p, b), jj) => {
              val w = trav(depth + 1, jj)
              (true, if(p) w++comma++b else w++b)
            }
          })._2 ++ rbracket
      , o =>
          lbrace ++ o.toList.reverse.foldLeft(false, Vector[Char]())({
            case ((p, b), (f, jj)) => {
              val w = ('"' +: Vector(f flatMap escape: _*) :+ '"') ++colon++trav(depth + 1, jj)
              (true, if(p) w++comma++b else w++b)
            }
          })._2 ++ rbrace
      )
    }

    trav(0, j)
  }
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
  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  def nospace: PrettyParams =
    PrettyParams(
      _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    )

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  def pretty(indent: JsonWhitespaces): PrettyParams =
    PrettyParams(
      _ => Monoid[JsonWhitespaces].zero
    , n => JsonLine +: indent * n
    , n => JsonLine +: indent * (n - 1)
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
      , n => JsonLine +: indent * n
      , n => JsonLine +: indent * (n - 1)
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
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
   * The lens to the `rbraceLeft` configuration value.
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

}
