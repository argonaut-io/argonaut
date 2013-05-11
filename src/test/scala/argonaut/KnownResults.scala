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
    """[10]"""                                                    ! jSingleArray(jNumber(10))                                                                                             |
    """ [ 10 ] """                                                ! jSingleArray(jNumber(10))                                                                                             |
    """{"number":20}"""                                           ! jSingleObject("number", jNumber(20))                                                                                  |
    """{"firstKey":100,"secondKey":"secondValue"}"""              ! ("secondKey", jString("secondValue")) ->: ("firstKey", jNumber(100)) ->: jEmptyObject                                 |
    """{ "firstKey" : 100 , "secondKey" : "secondValue" }"""      ! ("secondKey", jString("secondValue")) ->: ("firstKey", jNumber(100)) ->: jEmptyObject                                 |
    """[100,"secondValue"]"""                                     ! jArray(List(jNumber(100), jString("secondValue")))                                                                    |
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
    "1"                                                           ! jNumber(1)                                                                                                            |
    "-1"                                                          ! jNumber(-1)                                                                                                           |
    "0"                                                           ! jNumber(0)                                                                                                            |
    "1E999"                                                       ! jNumber("1E999".toDouble)                                                                                             |
    "1E+999"                                                      ! jNumber("1E+999".toDouble)                                                                                            |
    "1E-999"                                                      ! jNumber("1E-999".toDouble)                                                                                            |
    "158699798998941697"                                          ! jNumber(158699798998941697D)

  def parseFailures =
    "JSON"                                            | "parse result"                                                                                |
    """[][]"""                                        ! "JSON contains invalid suffix content: []".left[Json]                                         |
    """{}{}"""                                        ! "JSON contains invalid suffix content: {}".left[Json]                                         |
    "\"\"\"\""                                        ! "JSON contains invalid suffix content: \"\"".left[Json]                                       |
    "\"test"                                          ! "JSON terminates unexpectedly".left[Json]                                                     |
    "[7,,]"                                           ! "Unexpected content found: ,]".left[Json]                                                     |
    """{"firstKey":100,"secondKey":}"""               ! "Unexpected content found: }".left[Json]                                                      |
    """{"firstKey":}"""                               ! "Unexpected content found: }".left[Json]                                                      |
    """{"firstKey"}"""                                ! "Expected field separator token but found: }".left[Json]                                      |
    """[[}]"""                                        ! "Unexpected content found: }]".left[Json]

}
