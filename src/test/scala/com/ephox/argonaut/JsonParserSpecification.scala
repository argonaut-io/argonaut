package com.ephox.argonaut

import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import scalaz._
import Scalaz._

object JsonParserSpecification extends Properties("JsonParser") {
  property("Known valid results parse to the expected structure.") =
    forAll(Gen.oneOf(KnownResults.validResultPairings)){pairing =>
      val actualParseResult = JsonParser.parse(pairing._1)
      ("actualParseResult = " + actualParseResult.shows) |: {
	actualParseResult === pairing._2.successNel[String]
      }
    }
  property("Known invalid results parse to the expected error.") =
    forAll(Gen.oneOf(KnownResults.parseFailures)){pairing =>
      val actualParseResult = JsonParser.parse(pairing._1)
      ("actualParseResult = " + actualParseResult.shows) |: {
	actualParseResult === pairing._2.fail[Json]
      }
    }
  property("Parsed, printed and then parsed again generates the same structure") =
    forAll(JsonGenerators.arrayOrObjectGenerator.map(_.toString).label("arrayOrObject")){json =>
      val firstParsed = JsonParser.parse(json)
      ("firstParsed = " + firstParsed) |: {
        val printedJSON = firstParsed.map(jsonValue => jsonValue.nospaces)
        ("printedJSON = " + printedJSON) |: {
          val secondParsed = printedJSON.flatMap(secondJSON => JsonParser.parse(secondJSON))
          ("secondParsed = " + secondParsed) |: (firstParsed === secondParsed && secondParsed.isSuccess)
        }
      }
    }
}
