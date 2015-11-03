package argonaut

import argonaut.Data._
import argonaut.JsonNumberScalaz._
import argonaut.JsonObjectScalaz._
import argonaut.JsonScalaz._
import monocle.law.discipline.PrismTests
import org.specs2.{ScalaCheck, Specification}

import scalaz.std.anyVal._
import scalaz.std.list._

object JsonMonocleSpecification extends Specification with ScalaCheck {

  def is = s2"""
  Prism
    JsonBoolean     ${PrismTests(JsonMonocle.jBoolPrism).all}
    JsonNumber      ${PrismTests(JsonMonocle.jNumberPrism).all}
    JsonArray       ${PrismTests(JsonMonocle.jArrayPrism).all}
    JsonObject      ${PrismTests(JsonMonocle.jObjectPrism).all}
    JsonBigDecimal  ${PrismTests(JsonMonocle.jBigDecimalPrism).all}
    JsonDouble      ${PrismTests(JsonMonocle.jDoublePrism).all}
    JsonFloat       ${PrismTests(JsonMonocle.jDoublePrism).all}
    JsonBigInt      ${PrismTests(JsonMonocle.jBigIntPrism).all}
    JsonLong        ${PrismTests(JsonMonocle.jLongPrism).all}
    JsonInt         ${PrismTests(JsonMonocle.jIntPrism).all}
    JsonShort       ${PrismTests(JsonMonocle.jShortPrism).all}
    JsonByte        ${PrismTests(JsonMonocle.jBytePrism).all}
  """
  
}
