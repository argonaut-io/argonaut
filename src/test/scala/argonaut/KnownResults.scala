package argonaut

import scalaz._
import Scalaz._
import Json._
import JsonObject._
import org.specs2.matcher.DataTables

object KnownResults extends DataTables {
  def validResultPairings = 
    "JSON"                                                        | "Typed Result"                                                                                                        |
    """[]"""                                                      ! jEmptyArray                                                                                                           |
    """{}"""                                                      ! jEmptyObject                                                                                                          |
    """[10]"""                                                    ! jSingleArray(jDouble(10))                                                                                             |
    """ [ 10 ] """                                                ! jSingleArray(jDouble(10))                                                                                             |
    """{"number":20}"""                                           ! jSingleObject("number", jDouble(20))                                                                                  |
    """{"firstKey":100,"secondKey":"secondValue"}"""              ! ("secondKey", jString("secondValue")) ->: ("firstKey", jDouble(100)) ->: jEmptyObject                                 |
    """{ "firstKey" : 100 , "secondKey" : "secondValue" }"""      ! ("secondKey", jString("secondValue")) ->: ("firstKey", jDouble(100)) ->: jEmptyObject                                 |
    """[100,"secondValue"]"""                                     ! jArray(List(jDouble(100), jString("secondValue")))                                                                    |
    """[[]]"""                                                    ! jSingleArray(jEmptyArray)                                                                                             |
    """[[[]]]"""                                                  ! jSingleArray(jSingleArray(jEmptyArray))                                                                               |
    """[[],[]]"""                                                 ! jArray(List(jEmptyArray, jEmptyArray))                                                                                |
    """[{},{}]"""                                                 ! jArray(List(jEmptyObject, jEmptyObject))                                                                              |
    """[[{}],[{}]]"""                                             ! jArray(List(jSingleArray(jEmptyObject), jSingleArray(jEmptyObject)))                                                  |
    """ [ [ { } ] , [ { } ] ] """                                 ! jArray(List(jSingleArray(jEmptyObject), jSingleArray(jEmptyObject)))                                                  |
    """"\t""""                                                    ! jString("\t")                                                                                                         |
    """"\b""""                                                    ! jString("\b")                                                                                                         |
    """"\f""""                                                    ! jString("\f")                                                                                                         |
    """"\n""""                                                    ! jString("\n")                                                                                                         |
    """"\r""""                                                    ! jString("\r")                                                                                                         |
    """"\\""""                                                    ! jString("\\")                                                                                                         |
    """"\/""""                                                    ! jString("/")                                                                                                          |
    """"\"""""                                                    ! jString("\"")                                                                                                         |
    "1"                                                           ! jDouble(1)                                                                                                            |
    "-1"                                                          ! jDouble(-1)                                                                                                           |
    "0"                                                           ! jDouble(0)                                                                                                            |
    "1E999"                                                       ! jDouble("1E999".toDouble)                                                                                             |
    "1E+999"                                                      ! jDouble("1E+999".toDouble)                                                                                            |
    "1E-999"                                                      ! jDouble("1E-999".toDouble)                                                                                            |
    "158699798998941697"                                          ! jDouble(158699798998941697D)

  def parseFailures = 
    "JSON"                                            | "parse result"                                                                                                    |
    """[][]"""                                        ! "JSON contains invalid suffix content: []".failNel[Json]                                         |
    """{}{}"""                                        ! "JSON contains invalid suffix content: {}".failNel[Json]                                         |
    "\"\"\"\""                                        ! "JSON contains invalid suffix content: \"\"".failNel[Json]                                       |
    "\"test"                                          ! "Expected string bounds but found: ".failNel[Json]                                               |
    "[7,,]"                                           ! "Unexpected content found: ,]".failNel[Json]                                                     |
    """{"firstKey":100,"secondKey":}"""               ! "Unexpected content found: }".failNel[Json]                                                      |
    """{"firstKey":}"""                               ! "Unexpected content found: }".failNel[Json]                                                      |
    """{"firstKey"}"""                                ! "Expected field separator token but found: }".failNel[Json]                                      |
    """[[}]"""                                        ! "Unexpected content found: }]".failNel[Json]

}
