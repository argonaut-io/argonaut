package argonaut

import argonaut.Data._
import monocle.law.PrismLaws
import org.specs2.{ScalaCheck, Specification}

import scalaz.std.anyVal._
import scalaz.std.list._
import JsonScalaz._,JsonNumberScalaz._,JsonObjectScalaz._

object JsonMonocleSpecification extends Specification with ScalaCheck {

  def is = s2"""
  Prism
    JsonBoolean     ${PrismLaws(JsonMonocle.jBoolPrism)}
    JsonNumber      ${PrismLaws(JsonMonocle.jNumberPrism)}
    JsonArray       ${PrismLaws(JsonMonocle.jArrayPrism)}
    JsonObject      ${PrismLaws(JsonMonocle.jObjectPrism)}
    JsonBigDecimal  ${PrismLaws(JsonMonocle.jBigDecimalPrism)}
    JsonDouble      ${PrismLaws(JsonMonocle.jDoublePrism)}
    JsonFloat       ${PrismLaws(JsonMonocle.jDoublePrism)}
    JsonBigInt      ${PrismLaws(JsonMonocle.jBigIntPrism)}
    JsonLong        ${PrismLaws(JsonMonocle.jLongPrism)}
    JsonInt         ${PrismLaws(JsonMonocle.jIntPrism)}
    JsonShort       ${PrismLaws(JsonMonocle.jShortPrism)}
    JsonByte        ${PrismLaws(JsonMonocle.jBytePrism)}
  """
  
}
