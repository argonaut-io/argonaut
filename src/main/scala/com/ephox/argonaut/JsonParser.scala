package com.ephox.argonaut

import util.parsing.combinator._
import Json._

class JsonParser extends Parsers {
  type Elem = Char

  def jobject: Parser[Json] = '{' ~> repsep(pair, ',') <~ '}' ^^ jsonObject

  def jarray: Parser[Json] = '[' ~> repsep(jvalue, ',') <~ ']' ^^ jsonArray

  def jvalue = jstring | jboolean | jnull | jarray | jnumber | jobject

  def jstring = string ^^ jsonString

  def jnumber = number ^^ jsonNumber

  def jnull = acceptSeq("null") ^^^ jsonNull

  def jboolean = (f | t) ^^ jsonBool

  //---------------------------------------------------------------------------

  def pair: Parser[(String, Json)] = (string <~ ':') ~ jvalue ^^ { case k ~ v => (k, v)}

  def f = acceptSeq("false") ^^^ false

  def t = acceptSeq("true") ^^^ true

  def string = '"' ~> chars <~ '"'

  def chars = (char*) ^^ {_.mkString}

  // has top be a Parser[List[Char]] as unicode char may be more than one java char...? not really sure how/if this hangs together
  def char =
          (escape ~ '\"' ^^^ "\""
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

  def number = (int | intfrac | intexp | intfracexp) ^^ {_.toString.toDouble}

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
  // FIX making this work failure is Parser[None] ????
  //  def choice[T](ps: List[Parser[T]]) = ps.foldRight(failure[T](""))(_ ||| _)

  // FIX has to be a better way...
  // FIX pull this out somewhere.
  def unicodeMaker(code: List[Char]) = {
    val i = Integer.parseInt(code toString, 16)
    val cs = Character.toChars(i)
    new String(cs)
  }
}
