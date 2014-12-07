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
    """[10]"""                                                    ! jSingleArray(jNumberOrNull(10))                                                                                       |
    """ [ 10 ] """                                                ! jSingleArray(jNumberOrNull(10))                                                                                       |
    """{"number":20}"""                                           ! jSingleObject("number", jNumberOrNull(20))                                                                            |
    """{"firstKey":100,"secondKey":"secondValue"}"""              ! ("secondKey", jString("secondValue")) ->: ("firstKey", jNumberOrNull(100)) ->: jEmptyObject                           |
    """{ "firstKey" : 100 , "secondKey" : "secondValue" }"""      ! ("secondKey", jString("secondValue")) ->: ("firstKey", jNumberOrNull(100)) ->: jEmptyObject                           |
    """[100,"secondValue"]"""                                     ! jArray(List(jNumberOrNull(100), jString("secondValue")))                                                              |
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
    "1"                                                           ! jNumberOrNull(1)                                                                                                      |
    "-1"                                                          ! jNumberOrNull(-1)                                                                                                     |
    "0"                                                           ! jNumberOrNull(0)                                                                                                      |
    "1E999"                                                       ! jNumberOrNull("1E999")                                                                                                |
    "1E+999"                                                      ! jNumberOrNull("1E+999")                                                                                               |
    "1E-999"                                                      ! jNumberOrNull("1E-999")                                                                                               |
    "158699798998941697"                                          ! jNumberOrNull(158699798998941697L)

  def parseFailures =
    "JSON"                                            | "parse result"                                                                                |
    """[][]"""                                        ! "JSON contains invalid suffix content: []".left[Json]                                         |
    """{}{}"""                                        ! "JSON contains invalid suffix content: {}".left[Json]                                         |
    "\"\"\"\""                                        ! "JSON contains invalid suffix content: \"\"".left[Json]                                       |
    "\"test"                                          ! "JSON terminates unexpectedly.".left[Json]                                                    |
    "[7,,]"                                           ! "Unexpected content found: ,]".left[Json]                                                     |
    """{"firstKey":100,"secondKey":}"""               ! "Unexpected content found: }".left[Json]                                                      |
    """{"firstKey":}"""                               ! "Unexpected content found: }".left[Json]                                                      |
    """{"firstKey"}"""                                ! "Expected field separator token but found: }".left[Json]                                      |
    """[[}]"""                                        ! "Unexpected content found: }]".left[Json]

}
