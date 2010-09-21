package com.ephox.argonaut

import util.parsing.combinator._
import util.parsing.input.CharSequenceReader
import Json._
import java.io.CharArrayReader

class JsonParser extends Parsers {
  type Elem = Char

  val xnull = acceptSeq("null") ^^^ jsonNull

//  val xboolean =  elem("true") ||| elem("false")

}



/*

expr = jsobject

jsobject = { [key: value,] [key,value] }

key: "blah"


*/
