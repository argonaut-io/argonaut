package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Gen
import Data._
import Argonaut._
import org.specs2._
import scalaz._
import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.scalacheck.ScalazArbitrary._
import monocle.law.LensLaws
import PrettyParamsScalaz._

object PrettyParamsMonocleSpecification extends Specification with ScalaCheck {
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
    lrbracketsEmpty   $lrbracketsEmptyLens
    arrayCommaLeft    $arrayCommaLeftLens
    arrayCommaRight   $arrayCommaRightLens
    objectCommaLeft   $objectCommaLeftLens
    objectCommaRight  $objectCommaRightLens
    colonLeft         $colonLeftLens
    colonRight        $colonRightLens
    preserveOrder     $preserveOrderLens
    dropNullKeys      $dropNullKeysLens
  """

  def lbraceLeftLens = LensLaws(PrettyParamsMonocle.lbraceLeftL)
  def lbraceRightLens = LensLaws(PrettyParamsMonocle.lbraceRightL)
  def rbraceLeftLens = LensLaws(PrettyParamsMonocle.rbraceLeftL)
  def rbraceRightLens = LensLaws(PrettyParamsMonocle.rbraceRightL)
  def lbracketLeftLens = LensLaws(PrettyParamsMonocle.lbracketLeftL)
  def lbracketRightLens = LensLaws(PrettyParamsMonocle.lbracketRightL)
  def rbracketLeftLens = LensLaws(PrettyParamsMonocle.rbracketLeftL)
  def rbracketRightLens = LensLaws(PrettyParamsMonocle.rbracketRightL)
  def lrbracketsEmptyLens = LensLaws(PrettyParamsMonocle.lrbracketsEmptyL)
  def arrayCommaLeftLens = LensLaws(PrettyParamsMonocle.arrayCommaLeftL)
  def arrayCommaRightLens = LensLaws(PrettyParamsMonocle.arrayCommaRightL)
  def objectCommaLeftLens = LensLaws(PrettyParamsMonocle.objectCommaLeftL)
  def objectCommaRightLens = LensLaws(PrettyParamsMonocle.objectCommaRightL)
  def colonLeftLens = LensLaws(PrettyParamsMonocle.colonLeftL)
  def colonRightLens = LensLaws(PrettyParamsMonocle.colonRightL)
  def preserveOrderLens = LensLaws(PrettyParamsMonocle.preserveOrderL)
  def dropNullKeysLens = LensLaws(PrettyParamsMonocle.dropNullKeysL)
}
