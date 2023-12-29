package argonaut

import argonaut.Data._
import argonaut.JsonMonocle._
import argonaut.JsonNumberCats._
import argonaut.JsonObjectCats._
import argonaut.JsonCats._
import monocle.function.Plated
import monocle.law.discipline.PrismTests
import monocle.law.discipline.TraversalTests

object JsonMonocleSpecification extends ArgonautSpec {

  // Retired for the moment:
  // JsonDouble      ${OptionalTests(JsonMonocle.jDoubleOptional).all}
  // JsonFloat       ${OptionalTests(JsonMonocle.jFloatOptional).all}
  def is = s2"""
  Prism
    JsonNull        ${PrismTests(JsonMonocle.jNullPrism).all}
    JsonBoolean     ${PrismTests(JsonMonocle.jBoolPrism).all}
    JsonNumber      ${PrismTests(JsonMonocle.jNumberPrism).all}
    JsonArray       ${PrismTests(JsonMonocle.jArrayPrism).all}
    JsonObject      ${PrismTests(JsonMonocle.jObjectPrism).all}
    JsonBigDecimal  ${PrismTests(JsonMonocle.jBigDecimalPrism).all}
    JsonBigInt      ${PrismTests(JsonMonocle.jBigIntPrism).all}
    JsonLong        ${PrismTests(JsonMonocle.jLongPrism).all}
    JsonInt         ${PrismTests(JsonMonocle.jIntPrism).all}
    JsonShort       ${PrismTests(JsonMonocle.jShortPrism).all}
    JsonByte        ${PrismTests(JsonMonocle.jBytePrism).all}
    descendants     ${TraversalTests(JsonMonocle.jDescendants).all}
  Plated
    JsonPlated      ${TraversalTests(Plated.plate[Json]).all}
  """

}
