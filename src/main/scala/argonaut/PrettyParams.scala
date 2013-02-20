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
  val lbraceLeft: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to right of a left brace.
   */
  val lbraceRight: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to left of a right brace.
   */
  val rbraceLeft: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to right of a right brace.
   */
  val rbraceRight: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to left of a left bracket.
   */
  val lbracketLeft: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to right of a left bracket.
   */
  val lbracketRight: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to left of a right bracket.
   */
  val rbracketLeft: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to right of a right bracket.
   */
  val rbracketRight: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to left of a comma.
   */
  val commaLeft: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to right of a comma.
   */
  val commaRight: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to left of a colon.
   */
  val colonLeft: Int => Vector[Char]

  /**
   * Takes the current depth and returns the spaces to insert to right of a colon.
   */
  val colonRight: Int => Vector[Char]

  /**
   * Returns a string representation of a pretty-printed JSON value.
   */
  def pretty(j: Json): String = lpretty(j).mkString

  private[this] final val openBraceText = Vector('{')
  private[this] final val closeBraceText = Vector('}')
  private[this] final val openArrayText = Vector('[')
  private[this] final val closeArrayText = Vector(']')
  private[this] final val commaText = Vector(',')
  private[this] final val colonText = Vector(':')
  private[this] final val nullText = Vector.empty[Char] ++ "null"
  private[this] final val trueText = Vector.empty[Char] ++ "true"
  private[this] final val falseText = Vector.empty[Char] ++ "false"
  private[this] final val stringEnclosureText = Vector('"')

  /**
   * Returns a `Vector[Char]` representation of a pretty-printed JSON value.
   */
  def lpretty(j: Json): Vector[Char] = {
    import Json._
    import StringEscaping._
    def encloseJsonString(jsonString: JsonString): Vector[Char] = {
      jsonString.foldLeft(stringEnclosureText)((working, char) => working ++ escape(char)) ++ stringEnclosureText
    }
    def trav(depth: Int, k: Json): Vector[Char] = {
      def lbrace(): Vector[Char] = {
        lbraceLeft(depth) ++ openBraceText ++ lbraceRight(depth + 1)
      }
      def rbrace(): Vector[Char] = {
        rbraceLeft(depth + 1) ++ closeBraceText ++ rbraceRight(depth)
      }
      def lbracket(): Vector[Char] = {
        lbracketLeft(depth) ++ openArrayText ++ lbracketRight(depth + 1)
      }
      def rbracket(): Vector[Char] = {
        rbracketLeft(depth + 1) ++ closeArrayText ++ rbracketRight(depth)
      }
      def comma(): Vector[Char] = {
        commaLeft(depth + 1) ++ commaText ++ commaRight(depth + 1)
      }
      def colon(): Vector[Char] = {
        colonLeft(depth + 1) ++ colonText ++ colonRight(depth + 1)
      }

      k.fold(
        nullText
        , bool => bool ? trueText | falseText
        , n => Vector.empty[Char] ++ n.shows
        , s => encloseJsonString(s)
        , e => e.map(subElement => trav(depth + 1, subElement)).intersperse(comma()).foldLeft(Vector(lbracket()))(_ :+ _).flatten ++ rbracket()
        , o => o.toList.map(pair => (encloseJsonString(pair._1) ++ colon ++ trav(depth + 1, pair._2))).intersperse(comma()).foldLeft(Vector(lbrace()))(_ :+ _).flatten ++ rbrace()
      )
    }

    trav(0, j)
  }
}

object StringEscaping {
  def escape(c: Char): Vector[Char] =
    c match {
      case '\\' => Vector('\\', '\\')
      case '"' => Vector('\\', '\"')
      case '\b' => Vector('\\', 'b')
      case '\f' => Vector('\\', 'f')
      case '\n' => Vector('\\', 'n')
      case '\r' => Vector('\\', 'r')
      case '\t' => Vector('\\', 't')
      case possibleUnicode if possibleUnicode.isControl => Vector.empty[Char] ++ "\\u%04x".format(possibleUnicode.toInt)
      case _ => Vector(c)
    }
}

object PrettyParams extends PrettyParamss {
  def apply(
             lbraceLeft0: Int => Vector[Char]
           , lbraceRight0: Int => Vector[Char]
           , rbraceLeft0: Int => Vector[Char]
           , rbraceRight0: Int => Vector[Char]
           , lbracketLeft0: Int => Vector[Char]
           , lbracketRight0: Int => Vector[Char]
           , rbracketLeft0: Int => Vector[Char]
           , rbracketRight0: Int => Vector[Char]
           , commaLeft0: Int => Vector[Char]
           , commaRight0: Int => Vector[Char]
           , colonLeft0: Int => Vector[Char]
           , colonRight0: Int => Vector[Char]
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
  private[this] val emptyCharVector: Vector[Char] = Vector.empty
  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  def nospace: PrettyParams =
    PrettyParams(
      _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    , _ => emptyCharVector
    )

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  def pretty(indent: Vector[Char]): PrettyParams =
    PrettyParams(
      _ => emptyCharVector
    , n => '\n' +: Vector.fill(n)(indent).flatten
    , n => '\n' +: Vector.fill(n - 1)(indent).flatten
    , _ => emptyCharVector
    , _ => emptyCharVector
    , n => '\n' +: Vector.fill(n)(indent).flatten
    , n => '\n' +: Vector.fill(n - 1)(indent).flatten
    , _ => emptyCharVector
    , _ => emptyCharVector
    , n => '\n' +: Vector.fill(n)(indent).flatten
    , _ => Vector(' ')
    , _ => Vector(' ')
    )

  /**
   * A pretty-printer configuration that indents by two spaces.
   */
  def spaces2: PrettyParams =
    pretty(Vector.fill(2)(' '))

  /**
   * A pretty-printer configuration that indents by four spaces.
   */
  def spaces4: PrettyParams =
    pretty(Vector.fill(4)(' '))
}