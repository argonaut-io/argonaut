package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Gen
import Data._
import Argonaut._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._


object PrettyParamsSpecification extends Specification with ScalaCheck {
  // Synthetic Equal implementations used for testing.
  implicit val intToStringEqual: Equal[Int => String] = new Equal[Int => String] {
    val indents: List[Int] = (0 to 5).toList :+ 100
    def equal(a1: Int => String, a2: Int => String): Boolean = {
      indents.map(a1) === indents.map(a2)
    }
  }
  implicit val prettyParamsEqual: Equal[PrettyParams] = new Equal[PrettyParams] {
    def equal(a1: PrettyParams, a2: PrettyParams): Boolean = {
      (a1.lbraceLeft === a2.lbraceLeft) &&
      (a1.lbraceRight === a2.lbraceRight) &&
      (a1.rbraceLeft === a2.rbraceLeft) &&
      (a1.rbraceRight === a2.rbraceRight) &&
      (a1.lbracketLeft === a2.lbracketLeft) &&
      (a1.lbracketRight === a2.lbracketRight) &&
      (a1.rbracketLeft === a2.rbracketLeft) &&
      (a1.rbracketRight === a2.rbracketRight) &&
      (a1.commaLeft === a2.commaLeft) &&
      (a1.commaRight === a2.commaRight) &&
      (a1.colonLeft === a2.colonLeft) &&
      (a1.colonRight === a2.colonRight) &&
      (a1.preserveOrder === a2.preserveOrder)
    }
  }

  val jsonSpacesMap: Map[Int, String] = Map(
    0 -> """{"key1":"value1","key2":[9,21,0]}""",
    2 -> """|{
            |  "key1" : "value1",
            |  "key2" : [
            |    9,
            |    21,
            |    0
            |  ]
            |}""".stripMargin,
    4 -> """|{
            |    "key1" : "value1",
            |    "key2" : [
            |        9,
            |        21,
            |        0
            |    ]
            |}""".stripMargin
  )

  def is = s2"""
  PrettyParams
    lbraceLeft
      LensLaws            $lbraceLeftLensLaws
      Indent              $lbraceLeftIndent
    lbraceRight
      LensLaws            $lbraceRightLensLaws
      Indent              $lbraceRightIndent
    rbraceLeft
      LensLaws            $rbraceLeftLensLaws
      Indent              $rbraceLeftIndent
    rbraceRight
      LensLaws            $rbraceRightLensLaws
      Indent              $rbraceRightIndent
    lbracketLeft
      LensLaws            $lbracketLeftLensLaws
      Indent              $lbracketLeftLensLaws
    lbracketRight
      LensLaws            $lbracketRightLensLaws
      Indent              $lbracketRightIndent
    rbracketLeft
      LensLaws            $rbracketLeftLensLaws
      Indent              $rbracketLeftLensLaws
    rbracketRightL
      LensLaws            $rbracketRightLensLaws
      Indent              $rbracketRightIndent
    commaLeft
      LensLaws            $commaLeftLensLaws
      Indent              $commaLeftIndent
    commaRight
      LensLaws            $commaRightLensLaws
      Indent              $commaRightIndent
    colonLeft
      LensLaws            $colonLeftLensLaws
      Indent              $colonLeftIndent
    colonRight
      LensLaws            $colonRightLensLaws
      Indent              $colonRightIndent
    preserveOrder
      LensLaws            $preserveOrderLensLaws
      Indent              $preserveOrderIndent
    nospaces/spaces2/spaces4
      SpacesComparison      $spacesComparison
    numberPrinting
      Whole               $numbersWhole
      Fractional          $numbersFractional
      """

  val lbraceLeftLensLaws = lens.laws(PrettyParams.lbraceLeftL)
  val lbraceLeftIndent = prop{(indent: String) =>
    val prettyParams = lbraceLeftL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """%s{"test":"value"}""".format(indent)
  }
  val lbraceRightLensLaws = lens.laws(PrettyParams.lbraceRightL)
  val lbraceRightIndent = prop{(indent: String) =>
    val prettyParams = lbraceRightL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{%s"test":"value"}""".format(indent)
  }
  val rbraceLeftLensLaws = lens.laws(PrettyParams.rbraceLeftL)
  val rbraceLeftIndent = prop{(indent: String) =>
    val prettyParams = rbraceLeftL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":"value"%s}""".format(indent)
  }
  val rbraceRightLensLaws = lens.laws(PrettyParams.rbraceRightL)
  val rbraceRightIndent = prop{(indent: String) =>
    val prettyParams = rbraceRightL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":"value"}%s""".format(indent)
  }
  val lbracketLeftLensLaws = lens.laws(PrettyParams.lbracketLeftL)
  val lbracketLeftIndent = prop{(indent: String) =>
    val prettyParams = lbracketLeftL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """%s[true,false]""".format(indent)
  }
  val lbracketRightLensLaws = lens.laws(PrettyParams.lbracketRightL)
  val lbracketRightIndent = prop{(indent: String) =>
    val prettyParams = lbracketRightL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[%strue,false]""".format(indent)
  }
  val rbracketLeftLensLaws = lens.laws(PrettyParams.lbracketRightL)
  val rbracketLeftIndent = prop{(indent: String) =>
    val prettyParams = rbracketLeftL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,false%s]""".format(indent)
  }
  val rbracketRightLensLaws = lens.laws(PrettyParams.rbracketRightL)
  val rbracketRightIndent = prop{(indent: String) =>
    val prettyParams = rbracketRightL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,false]%s""".format(indent)
  }
  val commaLeftLensLaws = lens.laws(PrettyParams.commaLeftL)
  val commaLeftIndent = prop{(indent: String) =>
    val prettyParams = commaLeftL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true%s,false]""".format(indent)
  }
  val commaRightLensLaws = lens.laws(PrettyParams.commaRightL)
  val commaRightIndent = prop{(indent: String) =>
    val prettyParams = commaRightL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,%sfalse]""".format(indent)
  }
  val colonLeftLensLaws = lens.laws(PrettyParams.colonLeftL)
  val colonLeftIndent = prop{(indent: String) =>
    val prettyParams = colonLeftL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test"%s:"value"}""".format(indent)
  }
  val colonRightLensLaws = lens.laws(PrettyParams.colonRightL)
  val colonRightIndent = prop{(indent: String) =>
    val prettyParams = colonRightL.set(PrettyParams.nospace, _ => indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":%s"value"}""".format(indent)
  }
  val preserveOrderLensLaws = lens.laws(PrettyParams.preserveOrderL)
  val preserveOrderIndent = prop{(preserve: Boolean, pairs: JsonAssocList) =>
    val prettyParams = preserveOrderL.set(PrettyParams.nospace, preserve)
    val json = prettyParams.pretty(jObjectAssocList(pairs)).parseOption.get
    if (preserve) {
      val pairsDeduplicated = pairs.foldLeft[JsonAssocList](List.empty){case (working, (key, value)) =>
        if (working.exists(_._1 === key)) {
          working.map{case (pKey, pValue) => (pKey, if (pKey === key) value else pValue)}
        } else {
          working :+ ((key, value))
        }
      }
      json.objectOrEmpty.toList aka "Order preserved list" must beEqualTo(pairsDeduplicated)
    } else {
      json.objectOrEmpty.toMap aka "Order ignoring map" must beEqualTo(pairs.toMap)
    }
  }
  val spacesComparison = forAllNoShrink(Gen.oneOf(0, 2, 4), Gen.oneOf(0, 2, 4)){(firstIndex, secondIndex) =>
    val json = jsonSpacesMap(firstIndex).parseOption.get
    val printedJson = secondIndex match {
      case 0 => json.nospaces
      case 2 => json.spaces2
      case 4 => json.spaces4
    }
    printedJson === jsonSpacesMap(secondIndex)
  } ^ end
  val numbers: Fragments = "number printing" ^
    "whole number pretty print" ! prop{(n: Long) =>
      jNumberOrNull(n).nospaces === n.toString
    } ^
    "fractional number pretty print" ! prop{(d: Double) =>
      jNumberOrNull(d).nospaces === d.toString
    } ^ end
}
