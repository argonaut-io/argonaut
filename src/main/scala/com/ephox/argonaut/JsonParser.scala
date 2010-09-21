package com.ephox.argonaut

import util.parsing.combinator._
import Json._

class JsonParser extends Parsers {
  type Elem = Char

  def jnull = acceptSeq("null") ^^^ jsonNull
  def jboolean = (jfalse ||| jtrue) ^^ jsonBool
  def jfalse = acceptSeq("false") ^^^ false
  def jtrue = acceptSeq("true") ^^^ true

  def number = int ||| (int ~ frac) ||| (int ~ exp) ||| (int ~ frac ~ exp) ^^ {
    case i:List[Elem] => java.lang.Double.valueOf(i.toString)
    case x:Any => error("todo: " + x)
  }


//  def jobject: Parser[Json] = acceptSeq("{}") ||| ('{' ~ members ~ '}')
//  def members = pair ||| (pair ~ ',' ~ members)
//  def pair = string ~ ':' ~ value
//  def array: Parser[Json] = acceptSeq("[]") ||| ('[' ~ elements ~ ']')
//  def elements = value ||| ('[' ~ elements ~ ']')
  def value = jstring  ||| jboolean ||| jnull // ||| array ||| jobject

  def jstring = string ^^ jsonString

  def string = ('"' ~> optionalChars <~ '"') ^^ { (l: List[Char]) => l.toString}


  def chars = char+
  def optionalChars = char*
  def char = basicchar ||| escapedchar
  def basicchar  = elem("char", (c: Char) => c != '"' && c != '\\')
  def escapedchar  = '\\' ~> escapecode ^^ {(c: Char) => c}
  def escapecode = '"' ||| '\\' ||| '/' ||| 'b' ||| 'f' ||| 'n' ||| 'r' ||| 't' ||| unicode
  def unicode: Parser[Char]  = ('u' ~ repN(4, hex)) ^^^ '?' // FIX evaluate....
  def hex = digit ||| 'a' ||| 'b' ||| 'c' ||| 'd' ||| 'e' ||| 'f'

  def int = digit ||| (digit1to9 ~ digits) ||| ('-' ~ digit) ||| ('-' ~ digit1to9 ~ digits)
  def frac = '.' ~ digits
  def exp = e ~ digits
  def digits = digit+
  def digit = '0' ||| digit1to9
  def digit1to9 = '1' ||| '2' ||| '3' ||| '4' ||| '5' ||| '6' ||| '7' ||| '8' ||| '9'
  def e = oneOf(List("e", "e+", "e-", "E", "E+", "E-"))


  def oneOf[ES](seqs: Iterable[ES])(implicit e : ES => Iterable[Elem]) = seqs map (acceptSeq[ES] _) reduceRight(_ ||| _)

}
