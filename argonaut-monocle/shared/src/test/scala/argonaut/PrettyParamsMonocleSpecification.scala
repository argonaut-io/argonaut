package argonaut

import argonaut.Data._
import argonaut.PrettyParamsCats._
import monocle.law.discipline.LensTests
import org.scalacheck.Arbitrary._

object PrettyParamsMonocleSpecification extends ArgonautSpec {
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

  def lbraceLeftLens = LensTests(PrettyParamsMonocle.lbraceLeft).all
  def lbraceRightLens = LensTests(PrettyParamsMonocle.lbraceRight).all
  def rbraceLeftLens = LensTests(PrettyParamsMonocle.rbraceLeft).all
  def rbraceRightLens = LensTests(PrettyParamsMonocle.rbraceRight).all
  def lbracketLeftLens = LensTests(PrettyParamsMonocle.lbracketLeft).all
  def lbracketRightLens = LensTests(PrettyParamsMonocle.lbracketRight).all
  def rbracketLeftLens = LensTests(PrettyParamsMonocle.rbracketLeft).all
  def rbracketRightLens = LensTests(PrettyParamsMonocle.rbracketRight).all
  def lrbracketsEmptyLens = LensTests(PrettyParamsMonocle.lrbracketsEmpty).all
  def arrayCommaLeftLens = LensTests(PrettyParamsMonocle.arrayCommaLeft).all
  def arrayCommaRightLens = LensTests(PrettyParamsMonocle.arrayCommaRight).all
  def objectCommaLeftLens = LensTests(PrettyParamsMonocle.objectCommaLeft).all
  def objectCommaRightLens = LensTests(PrettyParamsMonocle.objectCommaRight).all
  def colonLeftLens = LensTests(PrettyParamsMonocle.colonLeft).all
  def colonRightLens = LensTests(PrettyParamsMonocle.colonRight).all
  def preserveOrderLens = LensTests(PrettyParamsMonocle.preserveOrder).all
  def dropNullKeysLens = LensTests(PrettyParamsMonocle.dropNullKeys).all
}
