package argonaut

import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import scalaz._
import Scalaz._
import StringWrap._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import Data._

object JsonParserSpecification extends Specification with DataTables with ScalaCheck {
  def is = "parse" ^
    "Valid JSON parses into expected values" ! {
      KnownResults.validResultPairings |> {(json, expectedJSONValue) =>
        val actualParseResult = JsonParser.parse(json)
        actualParseResult === expectedJSONValue.successNel[String]
      }
    } ^
    "Invalid JSON parses into expected failures" ! {
      KnownResults.parseFailures |> {(json, parseResult) =>
        val actualParseResult = JsonParser.parse(json)
        actualParseResult === parseResult
      }
    } ^
    "Printed and then parsed again generates the same structure" ! prop{(json: Json) =>
      val printedJSON = json.nospaces
      ("printedJSON = " + printedJSON) |: {
        val parsed = printedJSON.parse
        ("parsed = " + parsed) |: parsed === json.successNel
      }
    } ^ end
}
