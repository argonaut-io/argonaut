package argonaut

import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import scalaz._
import Scalaz._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import Data._
import Argonaut._

object JsonParserSpecification extends Specification with DataTables with ScalaCheck {
  // Generates chunks of whitespace according to the not at all specified JSON specification.
  val whitespaceGen: Gen[String] = listOf(Gen.oneOf(' ', '\n', '\r', '\t')).map(_.mkString)

  val whitespaceObjectGen: Gen[String] = whitespaceGen.map(whitespace => """#{#"field1"#:#12#,#"field2"#:#"test"#}#""".replace("#", whitespace))
  val whitespaceObject: Json = ("field1" := 12.0d) ->: ("field2" := "test") ->: jEmptyObject

  val whitespaceArrayGen: Gen[String] = whitespaceGen.map(whitespace => """#[#"value1"#,#12#]#""".replace("#", whitespace))
  val whitespaceArray: Json = jArray(jString("value1") :: jDouble(12) :: Nil)

  def is = "parse" ^
    "Whitespace is handled correctly for an object" ! forAllNoShrink(whitespaceObjectGen){json =>
      val parseResult = JsonParser.parse(json)
      ("parseResult = " + parseResult) |: 
      ("whitespaceObject = " + whitespaceObject) |:
      parseResult === whitespaceObject.right[String]
    } ^
    "Whitespace is handled correctly for an array" ! forAllNoShrink(whitespaceArrayGen){json =>
      val parseResult = JsonParser.parse(json)
      ("parseResult = " + parseResult) |: 
      ("whitespaceArray = " + whitespaceArray) |:
      parseResult === whitespaceArray.right[String]
    } ^
    "Valid JSON parses into expected values" ! {
      KnownResults.validResultPairings |> {(json, expectedJSONValue) =>
        val actualParseResult = JsonParser.parse(json)
        actualParseResult === expectedJSONValue.right[String]
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
        ("parsed = " + parsed) |: parsed === json.right
      }
    } ^ end
}
