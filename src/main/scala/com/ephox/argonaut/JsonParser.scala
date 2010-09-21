package com.ephox.argonaut

import util.parsing.combinator._
import Json._

class JsonParser extends Parsers {
  type Elem = Char

  val jsonBoolString: List[Char] => Json = (s: List[Char]) => jsonBool(s == "true".toList)

  val jnull = acceptSeq("null") ^^^ jsonNull

  val jboolean = oneOf(List("true", "false")) ^^ jsonBoolString

  val e = oneOf(List("e", "e+", "e-", "E", "E+", "E-"))

  val digit1to9 = '1' ||| '2' ||| '3' ||| '4' ||| '5' ||| '6' ||| '7' ||| '8' ||| '9'

  val digit = '0' ||| digit1to9

  val digits = digit +

  val exp = e ~ digits

  val frac = '.' ~ digits

  val int = digit ||| (digit1to9 ~ digits) ||| ('-' ~ digit) ||| ('-' ~ digit1to9 ~ digits)

  val jnumber = int ||| (int ~ frac) ||| (int ~ exp) ||| (int ~ frac ~ exp) ^^ {
    case i:List[Elem] => java.lang.Double.valueOf(i.toString)
    case x:Any => error("todo: " + x)
  }


  def oneOf[ES](seqs: Iterable[ES])(implicit e : ES => Iterable[Elem]) = seqs map (acceptSeq[ES] _) reduceRight(_ ||| _)

}
