package com.ephox.argonaut

import util.parsing.combinator._
import Json._
import util.parsing.input.CharSequenceReader

class JsonParser extends Parsers with ParserTools {
  type Elem = Char

  def jobject: Parser[Json] = openobject ~> repsep(pair, separator) <~ trailingcomma <~ closeobject ^^ jsonObject

  def jarray: Parser[Json] = openarray ~> repsep(jvalue, separator) <~ trailingcomma <~ closearray ^^ jsonArray

  def jvalue: Parser[Json] = whitespace ~> (jobject ||| jarray ||| jstring ||| jboolean ||| jnull |||  jnumber) <~ whitespace

  def jstring = string ^^ jsonString

  def jnumber = number ^^ jsonNumber

  def jnull = acceptSeq("null") ^^^ jsonNull

  def jboolean = (f | t) ^^ jsonBool

  //---------------------------------------------------------------------------

  // FIX: This is non-standard. Pull out functions to make building custom parsers easy. i.e. with or without non-standard hacks.
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

  // FIX has to be a Parser[List[Char]] as unicode char may be more than one java char...? not really sure how/if this hangs together
  // FIX need to test for escape sequences
  // FIX also how does this work for emit
  def char =
          (elem("char", List('\\', '"') contains (_: Char)) ^^ (_.toString)
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

  // FIX has to be a better way...
  // FIX pull this out somewhere.
  def unicodeMaker(code: List[Char]) = {
    val i = Integer.parseInt(code mkString, 16)
    val cs = Character.toChars(i)
    cs.mkString
  }
}

object JsonParser {

  // todo not the most general but hey?
  // todo name?
  def makeParser[X](s: String, success: Json => X, nosuccess: String => X) = {
    val p = new JsonParser
    val r = new CharSequenceReader(s)
    p.jvalue(r) match {
      case p.Success(j, _) => success(j)
      case p.Error(e, _) => nosuccess(e)
      case p.Failure(e, _) => nosuccess(e)
    }
  }

  def parse(s: String) = {
    val p = new JsonParser
    val r = new CharSequenceReader(s)
    p.jvalue(r)
  }

  def parseOptional(s: String) = {
    makeParser(s, Some(_), _ => None)
  }


  def parseTo[T](i: Json => T, s: String) = parse(s).map(i.apply(_))

  def parseToOrDie[T](i: Json => T, s: String): T = {
    val r = parseTo(i, s)
    if (r.successful) r.get else throw new ArgonautException
  }

  // FIX 17924 27/09/2010 these are the java versions...
  /*
  def parseTo[T](i: Interpret[T], s: String) = parse(s).map(i.apply(_))

  def parseToOrDie[T](i: Interpret[T], s: String): T = {
    val r = parseTo(i, s)
    if (r.successful) r.get else throw new ArgonautException
  }
  */
}
