package argonaut

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import Data._
import Argonaut._

object PrettyParamsSpecification extends ArgonautSpec {
  val jsonSpacesMap: Map[Int, String] = Map(
    0 -> """{"key1":"value1","key2":[9,21,0],"key2a":[],"key3":null}""",
    2 -> """|{
            |  "key1" : "value1",
            |  "key2" : [
            |    9,
            |    21,
            |    0
            |  ],
            |  "key2a" : [],
            |  "key3" : null
            |}""".stripMargin,
    4 -> """|{
            |    "key1" : "value1",
            |    "key2" : [
            |        9,
            |        21,
            |        0
            |    ],
            |    "key2a" : [],
            |    "key3" : null
            |}""".stripMargin
  )

  def is = s2"""
  Indentation
    lbraceLeft        $lbraceLeftIndent
    lbraceRight       $lbraceRightIndent
    rbraceLeft        $rbraceLeftIndent
    rbraceRight       $rbraceRightIndent
    lbracketLeft      $lbracketLeftIndent
    lbracketRight     $lbracketRightIndent
    rbracketLeft      $rbracketLeftIndent
    rbracketRight     $rbracketRightIndent
    lrbracketsEmpty   $lrbracketsEmptyIndent
    arrayCommaLeft    $arrayCommaLeftIndent
    arrayCommaRight   $arrayCommaRightIndent
    objectCommaLeft   $objectCommaLeftIndent
    objectCommaRight  $objectCommaRightIndent
    colonLeft         $colonLeftIndent
    colonRight        $colonRightIndent
    colonRight        $colonRightIndent

  Order Preservation
    preserve field order    $preserveOrder

  Spaces Comparison
    nospaces/spaces2/spaces4  $spacesComparison

  Number Printing
    whole number pretty print       $numberPrintingWholeNumber
    fractional number pretty print  $numberPrintingFractionalNumber

  Null keys
    null keys are present when all keys are null       ${nullKeys.onlyNullKey}
    null keys are present when first key is null       ${nullKeys.firstKeyNull}
    null keys are present when middle key is null      ${nullKeys.middleKeyNull}
    null keys are present when last key is null        ${nullKeys.lastKeyNull}
    null keys are not present when all keys are null   ${noNullKeys.onlyNullKey}
    null keys are not present when first key is null   ${noNullKeys.firstKeyNull}
    null keys are not present when middle key is null  ${noNullKeys.middleKeyNull}
    null keys are not present when last key is null    ${noNullKeys.lastKeyNull}
  """

  def lbraceLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(lbraceLeft = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """%s{"test":"value"}""".format(indent)
  }
  def lbraceRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(lbraceRight = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{%s"test":"value"}""".format(indent)
  }
  def rbraceLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(rbraceLeft = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":"value"%s}""".format(indent)
  }
  def rbraceRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(rbraceRight = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":"value"}%s""".format(indent)
  }
  def lbracketLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(lbracketLeft = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """%s[true,false]""".format(indent)
  }
  def lbracketRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(lbracketRight = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[%strue,false]""".format(indent)
  }
  def rbracketLeftIndent =  prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(rbracketLeft = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,false%s]""".format(indent)
  }
  def rbracketRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(rbracketRight = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,false]%s""".format(indent)
  }
  def lrbracketsEmptyIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(lrbracketsEmpty = indent)
    prettyParams.pretty(jEmptyArray) === """[%s]""".format(indent)
  }
  def arrayCommaLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(arrayCommaLeft = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true%s,false]""".format(indent)
  }
  def arrayCommaRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(arrayCommaRight = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,%sfalse]""".format(indent)
  }
  def objectCommaLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(preserveOrder = true, objectCommaLeft = indent)
    prettyParams.pretty(("test" := "value") ->: ("test2" := "value2") ->: jEmptyObject) === """{"test":"value"%s,"test2":"value2"}""".format(indent)
  }
  def objectCommaRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(preserveOrder = true, objectCommaRight = indent)
    prettyParams.pretty(("test" := "value") ->: ("test2" := "value2") ->: jEmptyObject) === """{"test":"value",%s"test2":"value2"}""".format(indent)
  }
  def colonLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(colonLeft = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test"%s:"value"}""".format(indent)
  }
  def colonRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(colonRight = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":%s"value"}""".format(indent)
  }
  def preserveOrder = prop{(preserve: Boolean, pairs: JsonAssocList) =>
    val prettyParams = PrettyParams.nospace.copy(preserveOrder = preserve)
    val json = prettyParams.pretty(jObjectAssocList(pairs)).parseOption.get
    if (preserve) {
      val pairsDeduplicated = pairs.foldLeft[JsonAssocList](List.empty){case (working, (key, value)) =>
        if (working.exists(_._1 == key)) {
          working.map{case (pKey, pValue) => (pKey, if (pKey == key) value else pValue)}
        } else {
          working :+ ((key, value))
        }
      }
      json.objectOrEmpty.toList aka "Order preserved list" must beEqualTo(pairsDeduplicated)
    } else {
      json.objectOrEmpty.toMap aka "Order ignoring map" must beEqualTo(pairs.toMap)
    }
  }
  def spacesComparison = forAllNoShrink(Gen.oneOf(0, 2, 4), Gen.oneOf(0, 2, 4)){(firstIndex, secondIndex) =>
    val json = jsonSpacesMap(firstIndex).parseOption.get
    val printedJson = secondIndex match {
      case 0 => PrettyParams.nospace.copy(preserveOrder = true).pretty(json)
      case 2 => PrettyParams.spaces2.copy(preserveOrder = true).pretty(json)
      case 4 => PrettyParams.spaces4.copy(preserveOrder = true).pretty(json)
    }
    printedJson === jsonSpacesMap(secondIndex)
  }
  def numberPrintingWholeNumber = prop{(n: Long) =>
    jNumber(n).nospaces === n.toString
  }
  def numberPrintingFractionalNumber = forAll(arbitrary[(Double, Double)].filter{case (first, second) => second != 0}.map(pair => pair._1 / pair._2).filter(d => d != d.floor)){d =>
    jNumberOrNull(d).nospaces === d.toString
  }
  val nullKeys = NullKey(
      dropNullKeys = false
    , onlyNullKeyExpected = """{"testNullKey":null}"""
    , firstKeyNullExpected = """{"testNullKey":null,"test":"value","testTwo":"valueTwo"}"""
    , middleKeyNullExpected = """{"test":"value","testNullKey":null,"testTwo":"valueTwo"}"""
    , lastKeyNullExpected = """{"test":"value","testTwo":"valueTwo","testNullKey":null}"""
  )
  val noNullKeys = {
    val result = """{"test":"value","testTwo":"valueTwo"}"""
    NullKey(
      dropNullKeys = true
    , onlyNullKeyExpected = """{}"""
    , firstKeyNullExpected = result
    , middleKeyNullExpected = result
    , lastKeyNullExpected = result
    )
  }
  case class NullKey(dropNullKeys: Boolean, onlyNullKeyExpected: String, firstKeyNullExpected: String, middleKeyNullExpected: String, lastKeyNullExpected: String) {
    val prettyParams = PrettyParams.nospace.copy(preserveOrder = true, dropNullKeys = dropNullKeys)

    def onlyNullKey = prettyParams.pretty(("testNullKey" := jNull) ->: jEmptyObject) === onlyNullKeyExpected
    def firstKeyNull = prettyParams.pretty(("testNullKey" := jNull) ->: ("test" := "value") ->: ("testTwo" := "valueTwo") ->: jEmptyObject) === firstKeyNullExpected
    def middleKeyNull = prettyParams.pretty(("test" := "value") ->: ("testNullKey" := jNull) ->: ("testTwo" := "valueTwo") ->: jEmptyObject) === middleKeyNullExpected
    def lastKeyNull = prettyParams.pretty(("test" := "value") ->: ("testTwo" := "valueTwo") ->: ("testNullKey" := jNull) ->: jEmptyObject) === lastKeyNullExpected
  }
}
