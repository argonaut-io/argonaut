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

  def is =
    lbraceLeft ^
    lbraceRight ^
    rbraceLeft ^
    rbraceRight ^
    lbracketLeft ^
    lbracketRight ^
    rbracketLeft ^
    rbracketRight ^
    commaLeft ^
    commaRight ^
    colonLeft ^
    colonRight ^
    preserveOrder ^
    spacesComparison ^
    numbers

  val lbraceLeft: Fragments = "lbraceLeft" ^
    "lens laws" ! lens.laws(PrettyParams.lbraceLeftL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = lbraceLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """%s{"test":"value"}""".format(indent)
    } ^ end
  val lbraceRight: Fragments = "lbraceRightL" ^
    "lens laws" ! lens.laws(PrettyParams.lbraceRightL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = lbraceRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{%s"test":"value"}""".format(indent)
    } ^ end
  val rbraceLeft: Fragments = "rbraceLeftL" ^
    "lens laws" ! lens.laws(PrettyParams.rbraceLeftL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = rbraceLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":"value"%s}""".format(indent)
    } ^ end
  val rbraceRight: Fragments = "rbraceRightL" ^
    "lens laws" ! lens.laws(PrettyParams.rbraceRightL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = rbraceRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":"value"}%s""".format(indent)
    } ^ end
  val lbracketLeft: Fragments = "lbracketLeftL" ^
    "lens laws" ! lens.laws(PrettyParams.lbracketLeftL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = lbracketLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) === """%s[true,false]""".format(indent)
    } ^ end
  val lbracketRight: Fragments = "lbracketRightL" ^
    "lens laws" ! lens.laws(PrettyParams.lbracketRightL) ^
     "indent" ! prop{(indent: String) =>
      val prettyParams = lbracketRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[%strue,false]""".format(indent)
    } ^ end
  val rbracketLeft: Fragments = "rbracketLeftL" ^
    "lens laws" ! lens.laws(PrettyParams.lbracketRightL) ^
     "indent" ! prop{(indent: String) =>
      val prettyParams = rbracketLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,false%s]""".format(indent)
    } ^ end
  val rbracketRight: Fragments = "rbracketRightL" ^
    "lens laws" ! lens.laws(PrettyParams.rbracketRightL) ^
     "indent" ! prop{(indent: String) =>
      val prettyParams = rbracketRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,false]%s""".format(indent)
    } ^ end
  val commaLeft: Fragments = "commaLeftL" ^
    "lens laws" ! lens.laws(PrettyParams.commaLeftL) ^
     "indent" ! prop{(indent: String) =>
      val prettyParams = commaLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true%s,false]""".format(indent)
    } ^ end
  val commaRight: Fragments = "commaRightL" ^
    "lens laws" ! lens.laws(PrettyParams.commaRightL) ^
     "indent" ! prop{(indent: String) =>
      val prettyParams = commaRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) === """[true,%sfalse]""".format(indent)
    } ^ end
  val colonLeft: Fragments = "colonLeftL" ^
    "lens laws" ! lens.laws(PrettyParams.colonLeftL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = colonLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test"%s:"value"}""".format(indent)
    } ^ end
  val colonRight: Fragments = "colonRightL" ^
    "lens laws" ! lens.laws(PrettyParams.colonRightL) ^
    "indent" ! prop{(indent: String) =>
      val prettyParams = colonRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) === """{"test":%s"value"}""".format(indent)
    } ^ end
  val preserveOrder: Fragments = "preserveOrderL" ^
    "lens laws" ! lens.laws(PrettyParams.preserveOrderL) ^
    "order preservation" ! prop {(preserve: Boolean, pairs: List[(JsonField, Json)]) =>
      val prettyParams = preserveOrderL.set(PrettyParams.nospace, preserve)
      val json = prettyParams.pretty(jObjectAssocList(pairs)).parseOption.get
      if (preserve) {
        json.objectOrEmpty.toInsertionMap === InsertionMap.apply(pairs: _*)
      } else {
        json.objectOrEmpty.toMap === pairs.toMap
      }
    } ^ end
  val spacesComparison: Fragments = "nospaces/spaces2/spaces4" ! forAllNoShrink(Gen.oneOf(0, 2, 4), Gen.oneOf(0, 2, 4)){(firstIndex, secondIndex) =>
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
      jNumberOrNull(n).nospaces === "%.0f".format(n.toDouble)
    } ^
    "fractional number pretty print" ! forAll(arbitrary[(Double, Double)].filter{case (first, second) => second != 0}.map(pair => pair._1 / pair._2).filter(d => d != d.floor)){d =>
      jNumberOrNull(d).nospaces === d.toString
    } ^ end
}
