package com.ephox.argonaut

import scalaz._
import Scalaz._
import Json._
import JsonObject._

object KnownResults {
  val validResultPairings: List[(String, Json)] = List(
    ("""[]""", jEmptyArray),
    ("""{}""", jEmptyObject),
    ("""[10]""", jSingleArray(jDouble(10))),
    ("""{"number":20}""", jSingleObject("number", jDouble(20))),
    ("""{"firstKey":100,"secondKey":"secondValue"}""", ("secondKey", jString("secondValue")) ->: ("firstKey", jDouble(100)) ->: jEmptyObject),
    ("""[100,"secondValue"]""", jArray(List(jDouble(100), jString("secondValue")))),
    ("""[[]]""", jSingleArray(jEmptyArray)),
    ("""[[[]]]""", jSingleArray(jSingleArray(jEmptyArray))),
    ("""[[],[]]""", jArray(List(jEmptyArray, jEmptyArray))),
    ("""[{},{}]""", jArray(List(jEmptyObject, jEmptyObject))),
    ("""[[{}],[{}]]""", jArray(List(jSingleArray(jEmptyObject), jSingleArray(jEmptyObject)))),
    (""""\t"""", jString("\t")),
    (""""\b"""", jString("\b")),
    (""""\f"""", jString("\f")),
    (""""\n"""", jString("\n")),
    (""""\r"""", jString("\r")),
    (""""\\"""", jString("\\")),
    (""""\/"""", jString("/")),
    (""""\""""", jString("\"")),
    ("158699798998941697", jDouble(158699798998941697D))
  )
    
  val parseFailures: List[(String, NonEmptyList[String])] = List(
    ("""[][]""", NonEmptyList("JSON contains invalid suffix content: []")),
    ("""{}{}""", NonEmptyList("JSON contains invalid suffix content: {}")),
    ("\"\"\"\"", NonEmptyList("JSON contains invalid suffix content: \"\"")),
    ("\"test", NonEmptyList("Expected string bounds but found: ")),
    ("[7,,]", NonEmptyList("Unexpected content found: ,]")),
    ("""{"firstKey":100,"secondKey":}""", NonEmptyList("Unexpected content found: }")),
    ("""{"firstKey":}""", NonEmptyList("Unexpected content found: }")),
    ("""{"firstKey"}""", NonEmptyList("Expected field separator token but found: }")),
    ("""[[}]""", NonEmptyList("Unexpected content found: }]"))
  )
}
