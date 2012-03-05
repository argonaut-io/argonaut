package com.ephox
package argonaut

import util.parsing.combinator._
import Json._

class JsonParser extends Parsers {
  type Elem = Char

  def jobject: Parser[Json] = openobject ~> repsep(pair, separator) <~ trailingcomma <~ closeobject ^^ jObject

  def jarray: Parser[Json] = openarray ~> repsep(jvalue, separator) <~ trailingcomma <~ closearray ^^ jArray

  def jvalue: Parser[Json] = whitespace ~> (jobject ||| jarray ||| jstring ||| jboolean ||| jnull |||  jnumber) <~ whitespace

  def jstring = string ^^ jString

  def jnumber = number ^^ jNumber

  def jnull = acceptSeq("null") ^^^ jNull

  def jboolean = (f | t) ^^ jBool

  def trailingcomma = ((whitespace ~ ',')?)

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

  def whitespaceChar = elem("whitespace", Character.isWhitespace(_))

  def whitespace = (whitespaceChar*)

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

  def unicode = repN(4, hex) flatMap (code =>
    try {
      val i = Integer.parseInt(code mkString, 16)
      val cs = Character.toChars(i)
      success(cs.mkString)
    } catch {
      case e => failure(e.toString)
    })

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

  def e = List("e", "e+", "e-", "E", "E+", "E-") map (acceptSeq (_:String)) reduceRight (_ ||| _)
}
