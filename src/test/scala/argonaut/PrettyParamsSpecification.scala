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
import monocle.LensLaws

object PrettyParamsSpecification extends Specification with ScalaCheck {
  // Synthetic Equal implementations used for testing.
  implicit val intToStringEqual: Equal[Int => String] = new Equal[Int => String] {
    val indents: List[Int] = (0 to 5).toList :+ 100
    def equal(a1: Int => String, a2: Int => String): Boolean = {
      indents.map(a1) === indents.map(a2)
    }
  }
  implicit val prettyParamsEqual: Equal[PrettyParams] = new Equal[PrettyParams] {
    def equal(a1: PrettyParams, a2: PrettyParams): Boolean = a1 == a2
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
  Lenses
    lbraceLeft        $lbraceLeftLens
    lbraceRight       $lbraceRightLens
    rbraceLeft        $rbraceLeftLens
    rbraceRight       $rbraceRightLens
    lbracketLeft      $lbracketLeftLens
    lbracketRight     $lbracketRightLens
    rbracketLeft      $rbracketLeftLens
    rbracketRight     $rbracketRightLens
    commaLeft         $commaLeftLens
    commaRight        $commaRightLens
    colonLeft         $colonLeftLens
    colonRight        $colonRightLens
    preserveOrder     $preserveOrderLens

  Indentation
    lbraceLeft        $lbraceLeftIndent
    lbraceRight       $lbraceRightIndent
    rbraceLeft        $rbraceLeftIndent
    rbraceRight       $rbraceRightIndent
    lbracketLeft      $lbracketLeftIndent
    lbracketRight     $lbracketRightIndent
    rbracketLeft      $rbracketLeftIndent
    rbracketRight     $rbracketRightIndent
    commaLeft         $commaLeftIndent
    commaRight        $commaRightIndent
    colonLeft         $colonLeftIndent
    colonRight        $colonRightIndent

  Order Preservation
    preserve field order    $orderPreservation

  Spaces Comparison
    nospaces/spaces2/spaces4  $spacesComparison

  Number Printing
    whole number pretty print       $numberPrintingWholeNumber
    fractional number pretty print  $numberPrintingFractionalNumber
  """

  def lbraceLeftLens = LensLaws(PrettyParams.lbraceLeftL)
  def lbraceRightLens = LensLaws(PrettyParams.lbraceRightL)
  def rbraceLeftLens = LensLaws(PrettyParams.rbraceLeftL)
  def rbraceRightLens = LensLaws(PrettyParams.rbraceRightL)
  def lbracketLeftLens = LensLaws(PrettyParams.lbracketLeftL)
  def lbracketRightLens = LensLaws(PrettyParams.lbracketRightL)
  def rbracketLeftLens = LensLaws(PrettyParams.rbracketLeftL)
  def rbracketRightLens = LensLaws(PrettyParams.rbracketRightL)
  def commaLeftLens = LensLaws(PrettyParams.commaLeftL)
  def commaRightLens = LensLaws(PrettyParams.commaRightL)
  def colonLeftLens = LensLaws(PrettyParams.colonLeftL)
  def colonRightLens = LensLaws(PrettyParams.colonRightL)
  def preserveOrderLens = LensLaws(PrettyParams.preserveOrderL)

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
  def commaLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(commaLeft = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true%s,false]""".format(indent)
  }
  def commaRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(commaRight = indent)
    prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,%sfalse]""".format(indent)
  }
  def colonLeftIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(colonLeft = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test"%s:"value"}""".format(indent)
  }
  def colonRightIndent = prop{(indent: String) =>
    val prettyParams = PrettyParams.nospace.copy(colonRight = indent)
    prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":%s"value"}""".format(indent)
  }
  def orderPreservation = prop {(preserve: Boolean, pairs: List[(JsonField, Json)]) =>
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
  def spacesComparison = forAllNoShrink(Gen.oneOf(0, 2, 4), Gen.oneOf(0, 2, 4)){(firstIndex, secondIndex) =>
    val json = jsonSpacesMap(firstIndex).parseOption.get
    val printedJson = secondIndex match {
      case 0 => json.nospaces
      case 2 => json.spaces2
      case 4 => json.spaces4
    }
    printedJson === jsonSpacesMap(secondIndex)
  }
  def numberPrintingWholeNumber = prop{(n: Long) =>
    jNumberOrNull(n).nospaces === "%.0f".format(n.toDouble)
  }
  def numberPrintingFractionalNumber = forAll(arbitrary[(Double, Double)].filter{case (first, second) => second != 0}.map(pair => pair._1 / pair._2).filter(d => d != d.floor)){d =>
    jNumberOrNull(d).nospaces === d.toString
  }
}
