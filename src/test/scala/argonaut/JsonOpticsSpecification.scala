package argonaut

import argonaut.Data._
import monocle.law.discipline.PrismTests
import org.specs2.{ScalaCheck, Specification}

import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.std.list._


object JsonOpticsSpecification extends Specification with ScalaCheck {

  def is = s2"""
  Prism
    JsonBoolean     ${PrismTests(Json.jBoolPrism).all}
    JsonNumber      ${PrismTests(Json.jNumberPrism).all}
    JsonArray       ${PrismTests(Json.jArrayPrism).all}
    JsonObject      ${PrismTests(Json.jObjectPrism).all}

    JsonBigDecimal  ${PrismTests(Json.jBigDecimalPrism).all}
    JsonDouble      ${PrismTests(Json.jDoublePrism).all}
    JsonFloat       ${PrismTests(Json.jDoublePrism).all}
    JsonBigInt      ${PrismTests(Json.jBigIntPrism).all}
    JsonLong        ${PrismTests(Json.jLongPrism).all}
    JsonInt         ${PrismTests(Json.jIntPrism).all}
    JsonShort       ${PrismTests(Json.jShortPrism).all}
    JsonByte        ${PrismTests(Json.jBytePrism).all}
  """

}
