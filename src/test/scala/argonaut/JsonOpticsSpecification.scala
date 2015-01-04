package argonaut

import argonaut.Data._
import monocle.law.PrismLaws
import org.specs2.{ScalaCheck, Specification}

import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.std.list._


object JsonOpticsSpecification extends Specification with ScalaCheck {

  def is = s2"""
  Prism
    JsonBoolean     ${PrismLaws(Json.jBoolPrism)}
    JsonNumber      ${PrismLaws(Json.jNumberPrism)}
    JsonArray       ${PrismLaws(Json.jArrayPrism)}
    JsonObject      ${PrismLaws(Json.jObjectPrism)}

    JsonBigDecimal  ${PrismLaws(Json.jBigDecimalPrism)}
    JsonDouble      ${PrismLaws(Json.jDoublePrism)}
    JsonFloat       ${PrismLaws(Json.jDoublePrism)}
    JsonBigInt      ${PrismLaws(Json.jBigIntPrism)}
    JsonLong        ${PrismLaws(Json.jLongPrism)}
    JsonInt         ${PrismLaws(Json.jIntPrism)}
    JsonShort       ${PrismLaws(Json.jShortPrism)}
    JsonByte        ${PrismLaws(Json.jBytePrism)}
  """
  
}