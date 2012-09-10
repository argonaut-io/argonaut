package com.ephox.argonaut

import scalaz._
import Scalaz._

// TODO: Fixme.
object KnownResults{} /* extends DataTables {
  def validResultPairings = 
    "JSON"                                            | "JSONValue"                                                                                                           |
    """[]"""                                          ! JSONArray()                                                                                                           |
    """{}"""                                          ! JSONObject()                                                                                                          |                                              
    """[10]"""                                        ! JSONArray(Seq(JSONNumber(10)))                                                                                        |
    """{"number":20}"""                               ! JSONObject(Map(JSONString("number") -> JSONNumber(20)))                                                               |
    """{"firstKey":100,"secondKey":"secondValue"}"""  ! new JSONObject(JSONString("firstKey") -> JSONNumber(100), JSONString("secondKey") -> JSONString("secondValue"))       |
    """[100,"secondValue"]"""                         ! JSONArray(Seq(JSONNumber(100), JSONString("secondValue")))                                                            |
    """[[]]"""                                        ! JSONArray(Seq(JSONArray()))                                                                                           |
    """[[[]]]"""                                      ! JSONArray(Seq(JSONArray(Seq(JSONArray()))))                                                                           |
    """[[],[]]"""                                     ! JSONArray(Seq(JSONArray(), JSONArray()))                                                                              |
    """[{},{}]"""                                     ! JSONArray(Seq(JSONObject(), JSONObject()))                                                                            |
    """[[{}],[{}]]"""                                 ! JSONArray(Seq(JSONArray(Seq(JSONObject())), JSONArray(Seq(JSONObject()))))                                            |
    """"\t""""                                        ! JSONString("\t")                                                                                                      |
    """"\b""""                                        ! JSONString("\b")                                                                                                      |
    """"\f""""                                        ! JSONString("\f")                                                                                                      |
    """"\n""""                                        ! JSONString("\n")                                                                                                      |
    """"\r""""                                        ! JSONString("\r")                                                                                                      |
    """"\\""""                                        ! JSONString("\\")                                                                                                      |
    """"\/""""                                        ! JSONString("/")                                                                                                       |
    """"\"""""                                        ! JSONString("\"")                                                                                                      |
    "158699798998941697"                              ! new JSONNumber(BigInt(158699798998941697l))
    
  def parseFailures = 
    "JSON"                                            | "parse result"                                                                                                    |
    """[][]"""                                        ! parseError("JSON contains invalid suffix content: []").failNel[JSONValue]                                         |
    """{}{}"""                                        ! parseError("JSON contains invalid suffix content: {}").failNel[JSONValue]                                         |
    "\"\"\"\""                                        ! parseError("JSON contains invalid suffix content: \"\"").failNel[JSONValue]                                       |
    "\"test"                                          ! parseError("Expected string bounds but found: ").failNel[JSONValue]                                               |
    "[7,,]"                                           ! parseError("Unexpected content found: ,]").failNel[JSONValue]                                                     |
    """{"firstKey":100,"secondKey":}"""               ! parseError("Unexpected content found: }").failNel[JSONValue]                                                      |
    """{"firstKey":}"""                               ! parseError("Unexpected content found: }").failNel[JSONValue]                                                      |
    """{"firstKey"}"""                                ! parseError("Expected field separator token but found: }").failNel[JSONValue]                                      |
    """[[}]"""                                        ! parseError("Unexpected content found: }]").failNel[JSONValue]
}*/