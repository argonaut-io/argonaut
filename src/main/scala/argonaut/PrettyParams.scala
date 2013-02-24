package argonaut

import scalaz._, Scalaz._
import annotation.tailrec

/**
 * Parameters for pretty-printing a JSON value.
 *
 * @author Tony Morris
 */
sealed trait PrettyParams {
  import PrettyParams._

  /**
   * Takes the current depth and returns the spaces to insert to left of a left brace.
   */
  val lbraceLeft: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to right of a left brace.
   */
  val lbraceRight: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to left of a right brace.
   */
  val rbraceLeft: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to right of a right brace.
   */
  val rbraceRight: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to left of a left bracket.
   */
  val lbracketLeft: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to right of a left bracket.
   */
  val lbracketRight: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to left of a right bracket.
   */
  val rbracketLeft: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to right of a right bracket.
   */
  val rbracketRight: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to left of a comma.
   */
  val commaLeft: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to right of a comma.
   */
  val commaRight: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to left of a colon.
   */
  val colonLeft: AppendIndent

  /**
   * Takes the current depth and returns the spaces to insert to right of a colon.
   */
  val colonRight: AppendIndent

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

  /**
   * Returns a string representation of a pretty-printed JSON value.
   */
  def pretty(j: Json): String = {
    import Json._
    import StringEscaping._
    def encloseJsonString(builder: StringBuilder, jsonString: JsonString): StringBuilder = {
      // TODO: Improve this so that it doesn't need to call escape for every single char.
      jsonString.foldLeft(builder.append(stringEnclosureText))((working, char) => working.append(escape(char))).append(stringEnclosureText)
    }
    def trav(builder: StringBuilder, depth: Int, k: Json): StringBuilder = {

      def lbrace(builder: StringBuilder): StringBuilder = {
        lbraceRight(lbraceLeft(builder, depth).append(openBraceText), depth + 1)
      }

      def rbrace(builder: StringBuilder): StringBuilder = {
        rbraceRight(rbraceLeft(builder, depth + 1).append(closeBraceText), depth)
      }

      def lbracket(builder: StringBuilder): StringBuilder = {
        lbracketRight(lbracketLeft(builder, depth).append(openArrayText), depth + 1)
      }

      def rbracket(builder: StringBuilder): StringBuilder = {
        rbracketRight(rbracketLeft(builder, depth + 1).append(closeArrayText), depth)
      }

      def comma(builder: StringBuilder): StringBuilder = {
        commaRight(commaLeft(builder, depth + 1).append(commaText), depth + 1)
      }

      def colon(builder: StringBuilder): StringBuilder = {
        colonRight(colonLeft(builder, depth + 1).append(colonText), depth + 1)
      }

      k.fold[StringBuilder](
        builder.append(nullText)
        , bool => builder.append(if (bool) trueText else falseText)
        , n => builder.append(n.shows)
        , s => encloseJsonString(builder, s)
        , e => {
          e.foldLeft((true, lbracket(builder))){case ((firstElement, builder), subElement) =>
            val withComma = if(firstElement) builder else comma(builder)
            val updatedBuilder = trav(withComma, depth + 1, subElement)
            (false, updatedBuilder)
          }._2 |> rbracket

          /*
          val elements: List[StringBuilder => StringBuilder] = e
            .map(subElement => (builder: StringBuilder) => trav(builder, depth + 1, subElement))
            .intersperse(b => comma(b))
          elements.foldLeft(lbracket(builder)){(builder, elem) =>
            elem(builder)
          }|> rbracket
          */
        }
        , o => {
          o.toList.foldLeft((true, lbrace(builder))){case ((firstElement, builder), (key, value)) =>
            val withComma = if(firstElement) builder else comma(builder)
            val updatedBuilder = trav(encloseJsonString(withComma, key) |> colon, depth + 1, value)
            (false, updatedBuilder)
          }._2 |> rbrace

          /*
          val elements: List[StringBuilder => StringBuilder] = o
            .toList
            .map(pair => (builder: StringBuilder) => (trav(colon(encloseJsonString(builder, pair._1)), depth + 1, pair._2)))
            .intersperse(b => comma(b))
          elements.foldLeft(lbrace(builder)){(builder, elem) =>
            elem(builder)
          }|> rbrace
          */
        }
      )
    }

    trav(new StringBuilder(), 0, j).toString()
  }

  /**
   * Returns a `Vector[Char]` representation of a pretty-printed JSON value.
   */
  def lpretty(j: Json): Vector[Char] = Vector.empty[Char] ++ pretty(j).toIndexedSeq
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
      case possibleUnicode if Character.isISOControl(possibleUnicode) => "\\u%04x".format(possibleUnicode.toInt)
      case _ => c.toString
    }
}

object PrettyParams extends PrettyParamss {
  type AppendIndent = (StringBuilder, Int) => StringBuilder
  def apply(
             lbraceLeft0: AppendIndent
           , lbraceRight0: AppendIndent
           , rbraceLeft0: AppendIndent
           , rbraceRight0: AppendIndent
           , lbracketLeft0: AppendIndent
           , lbracketRight0: AppendIndent
           , rbracketLeft0: AppendIndent
           , rbracketRight0: AppendIndent
           , commaLeft0: AppendIndent
           , commaRight0: AppendIndent
           , colonLeft0: AppendIndent
           , colonRight0: AppendIndent
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
  import PrettyParams._
  val builderIdentity: AppendIndent = (builder, n) => builder
  /**
   * A pretty-printer configuration that inserts no spaces.
   */
  def nospace: PrettyParams =
    PrettyParams(
      builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    , builderIdentity
    )

  @tailrec
  final def repeatAppend(stringBuilder: StringBuilder, toAppend: String, n: Int): StringBuilder = {
    if (n > 0) repeatAppend(stringBuilder.append(toAppend), toAppend, n - 1) else stringBuilder
  }

  /**
   * A pretty-printer configuration that indents by the given spaces.
   */
  def pretty(indent: String): PrettyParams =
    PrettyParams(
      builderIdentity
    , (builder, n) => repeatAppend(builder.append("\n"), indent, n)
    , (builder, n) => repeatAppend(builder.append("\n"), indent, n - 1)
    , builderIdentity
    , builderIdentity
    , (builder, n) => repeatAppend(builder.append("\n"), indent, n)
    , (builder, n) => repeatAppend(builder.append("\n"), indent, n - 1)
    , builderIdentity
    , builderIdentity
    , (builder, n) => repeatAppend(builder.append("\n"), indent, n)
    , (builder, n) => builder.append(" ")
    , (builder, n) => builder.append(" ")
    )

  /**
   * A pretty-printer configuration that indents by two spaces.
   */
  def spaces2: PrettyParams =
    pretty("  ")

  /**
   * A pretty-printer configuration that indents by four spaces.
   */
  def spaces4: PrettyParams =
    pretty("    ")
}