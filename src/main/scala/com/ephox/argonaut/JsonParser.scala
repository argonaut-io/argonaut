package com.ephox.argonaut

import util.parsing.combinator._
import Json._
import util.parsing.input.CharSequenceReader

class JsonParser extends Parsers {
  type Elem = Char

  val jsonParser = xnull

  val jsonBoolString: List[Char] => Json = (s: List[Char]) => jsonBool(s == "true".toList)

  val xnull = acceptSeq("null") ^^^ jsonNull

  val xboolean = (acceptSeq("true") ||| acceptSeq("false")) ^^ jsonBoolString


}

object JsonParser {
  def parseJson(s: String) = {
    val r = new CharSequenceReader(s)
    val p = new JsonParser
    p.jsonParser(r)
  }
}

