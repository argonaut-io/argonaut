package argonaut

import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Shrink._
import org.specs2._
import org.specs2.matcher._
import Data._
import Argonaut._

object JsonParserSpecification extends Specification with DataTables with ScalaCheck {
  // Generates chunks of whitespace according to the not at all specified JSON specification.
  val whitespaceGen: Gen[String] = listOf(Gen.oneOf(' ', '\n', '\r', '\t')).map(_.mkString)

  val whitespaceObjectGen: Gen[String] = whitespaceGen.map(whitespace => """#{#"field1"#:#12#,#"field2"#:#"test"#}#""".replace("#", whitespace))
  val whitespaceObject: Json = ("field1" := 12) ->: ("field2" := "test") ->: jEmptyObject

  val whitespaceArrayGen: Gen[String] = whitespaceGen.map(whitespace => """#[#"value1"#,#12#]#""".replace("#", whitespace))
  val whitespaceArray: Json = jArray(jString("value1") :: jNumberOrNull(12) :: Nil)

  def is = s2"""
  parse
    Whitespace is handled correctly for an object              $whitespaceForObject
    Whitespace is handled correctly for an array               $whitespaceForArray
    Valid JSON parses into expected values                     $validJson
    Invalid JSON parses into expected failures                 $invalidJson
    Printed and then parsed again generates the same structure $printParse
  """

  def whitespaceForObject =
    forAllNoShrink(whitespaceObjectGen) { json =>
      val parseResult = JsonParser.parse(json)
      ("parseResult = " + parseResult) |:
        ("whitespaceObject = " + whitespaceObject) |:
        parseResult == Right(whitespaceObject)
    }

  def whitespaceForArray =
    forAllNoShrink(whitespaceArrayGen) { json =>
      val parseResult = JsonParser.parse(json)
      ("parseResult = " + parseResult) |:
        ("whitespaceArray = " + whitespaceArray) |:
        parseResult == Right(whitespaceArray)
    }
  def validJson =
    KnownResults.validResultPairings |> { (json, expectedJSONValue) =>
      val actualParseResult = JsonParser.parse(json)
      actualParseResult == Right(expectedJSONValue)
    }

  def invalidJson =
    KnownResults.parseFailures |> { (json, parseResult) =>
      val actualParseResult = JsonParser.parse(json)
      actualParseResult == parseResult
    }

  def printParse =
    prop { (json: Json) =>
      val printedJSON = json.nospaces
      ("printedJSON = " + printedJSON) |: {
        val parsed = printedJSON.parse
        ("parsed = " + parsed) |: parsed == Right(json)
      }
    }
}
