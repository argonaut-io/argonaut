package com.ephox.argonaut

import util.parsing.combinator._
import Json._
import util.parsing.input.CharSequenceReader

class JsonParser extends Parsers {
  type Elem = Char

  def jobject: Parser[Json] = openobject ~> repsep(pair, separator) <~ trailingcomma <~ closeobject ^^ jsonObject

  def jarray: Parser[Json] = openarray ~> repsep(jvalue, separator) <~ trailingcomma <~ closearray ^^ jsonArray

  def jvalue = whitespace ~> (jobject ||| jarray ||| jstring ||| jboolean ||| jnull |||  jnumber) <~ whitespace  

  def jstring = string ^^ jsonString

  def jnumber = number ^^ jsonNumber

  def jnull = acceptSeq("null") ^^^ jsonNull

  def jboolean = (f | t) ^^ jsonBool

  //---------------------------------------------------------------------------

  def trailingcomma = ((whitespace ~ ',')?) // FIX: non standard, but common. Support, yay or nah?

  def openarray = '[' ~ whitespace

  def closearray = whitespace ~ ']'

  def openobject = '{' ~ whitespace

  def closeobject = whitespace ~ '}'

  def separator = whitespace ~ ',' ~ whitespace

  def colanSeparator = whitespace ~ ':' ~ whitespace

  def pair: Parser[(String, Json)] = (string <~ colanSeparator) ~ jvalue ^^ { case k ~ v => (k, v)}

  def f = acceptSeq("false") ^^^ false

  def t = acceptSeq("true") ^^^ true

  def string = '"' ~> chars <~ '"'

  def chars = (char*) ^^ {_.mkString}

  def whitespaceChar = elem("whitespace", c => Character.isWhitespace(c))

  def whitespace = (whitespaceChar*)

  // FIX has to be a Parser[List[Char]] as unicode char may be more than one java char...? not really sure how/if this hangs together
  // FIX need to test for escape sequences
  // FIX also how does this work for emit
  def char =
          (elem("char", (ch: Char) => ch != '\\' && ch != '"') ^^ { (ch: Char) => ch.toString }
          |escape ~ '\"' ^^^ "\""
    	    |escape ~ '\\' ^^^ "\\"
    	    |escape ~ '/'  ^^^ "/"
    	    |escape ~ 'b'  ^^^ "\b"
    	    |escape ~ 'f'  ^^^ "\f"
    	    |escape ~ 'n'  ^^^ "\n"
    	    |escape ~ 'r'  ^^^ "\r"
    	    |escape ~ 't'  ^^^ "\t"
    	    |escape ~> 'u' ~> unicode)

  def escape = '\\'

  def unicode = repN(4, hex) ^^ unicodeMaker

  def hex = digit | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'

  def number = (int ||| intfrac ||| intexp ||| intfracexp) ^^ {_.mkString.toDouble}

  def intexp = int ~ exp ^^ {case a ~ b => a ++ b}

  def intfracexp = int ~ frac ~ exp ^^ {case a ~ b ~ c => a ++ b ++ c}

  def intfrac = int ~ frac ^^ {case a ~ b => a ++ b}

  def exp = e ~ digits ^^ {case a ~ b => a ++ b}

  def frac = '.' ~ digits ^^ {case a ~ b => a :: b}

  def int = sign ~ (digitSeries | digitSingle) ^^ {
    case Some(s) ~ a => s :: a
    case None ~ a => a
  }

  def sign = ('-'?)

  def digits = digit+

  def digitSeries = nonzero ~ (digit*) ^^ {case a ~ b => a :: b}

  def digitSingle = digit map (List(_))

  def digit = elem("digit", _.isDigit)

  def nonzero = elem("nonzero", d => d.isDigit && d != '0')

  def e = choice(List("e", "e+", "e-", "E", "E+", "E-") map (acceptSeq (_:String)))

  // FIX talk with tony about ways to prevent this.... Alternative http://paste.pocoo.org/show/265567/
  def choice[T](ps: List[Parser[T]]) = ps.reduceRight(_ ||| _)

  // FIX has to be a better way...
  // FIX pull this out somewhere.
  def unicodeMaker(code: List[Char]) = {
    val i = Integer.parseInt(code mkString, 16)
    val cs = Character.toChars(i)
    cs.mkString
  }
}

object JsonParser {
  def parse(s: String) = {
    val p = new JsonParser()
    val r = new CharSequenceReader(s)
    p.jvalue(r)
  }
}
