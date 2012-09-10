package com.ephox.argonaut

import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import scalaz._
import Scalaz._

object AltParserTest extends Properties("AltParser") {
  /*
  def validResultsSpec = KnownResults.validResultPairings |> {(json, expectedJSONValue) =>
    val actualParseResult = AltParser.parse(json)
    actualParseResult must_== expectedJSONValue.successNel[JSONError]
  }
  def invalidResultsSpec = KnownResults.parseFailures |> {(json, parseResult) =>
    val actualParseResult = Parser.parse(json)
    actualParseResult === parseResult
  }
  */
  property("Parsed, printed and then parsed again generates the same structure") =
    forAll(JsonGenerators.arrayOrObjectGenerator.map(_.toString).label("arrayOrObject")){json =>
      val firstParsed = AltParser.parse(json)
      ("firstParsed = " + firstParsed) |: {
        val printedJSON = firstParsed.map(jsonValue => jsonValue.nospaces)
        ("printedJSON = " + printedJSON) |: {
          val secondParsed = printedJSON.flatMap(secondJSON => AltParser.parse(secondJSON))
          ("secondParsed = " + secondParsed) |: (firstParsed === secondParsed && secondParsed.isSuccess)
        }
      }
    }
}
