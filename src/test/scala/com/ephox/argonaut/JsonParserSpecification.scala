package com.ephox.argonaut

import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import scalaz._
import Scalaz._
import StringWrap._

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
  property("Printed and then parsed again generates the same structure") =
    forAll(JsonGenerators.jsonObjectOrArrayGenerator.label("arrayOrObject")){json =>
      val printedJSON = json.nospaces
      ("printedJSON = " + printedJSON) |: {
        val parsed = printedJSON.parse()
        ("parsed = " + parsed) |: parsed === json.successNel
      }
    }
}
