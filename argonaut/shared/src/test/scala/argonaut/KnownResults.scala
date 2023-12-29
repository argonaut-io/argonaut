package argonaut

import Json._
import org.specs2.matcher.DataTables

object KnownResults extends DataTables {
  // format: off
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
    "1E999"                                                       ! jNumber(BigDecimal("1E999"))                                                                                          |
    "1E+999"                                                      ! jNumber(BigDecimal("1E+999"))                                                                                         |
    "1E-999"                                                      ! jNumber(BigDecimal("1E-999"))                                                                                         |
    "158699798998941697"                                          ! jNumber(158699798998941697L)

  def parseFailures =
    "JSON"                                            | "parse result"                                                                           |
    """[][]"""                                        ! Left("JSON contains invalid suffix content: []")                                         |
    """{}{}"""                                        ! Left("JSON contains invalid suffix content: {}")                                         |
    "\"\"\"\""                                        ! Left("JSON contains invalid suffix content: \"\"")                                       |
    "\"test"                                          ! Left("JSON terminates unexpectedly.")                                                    |
    "[7,,]"                                           ! Left("Unexpected content found: ,]")                                                     |
    """{"firstKey":100,"secondKey":}"""               ! Left("Unexpected content found: }")                                                      |
    """{"firstKey":}"""                               ! Left("Unexpected content found: }")                                                      |
    """{"firstKey"}"""                                ! Left("Expected field separator token but found: }")                                      |
    """[[}]"""                                        ! Left("Unexpected content found: }]")

}
