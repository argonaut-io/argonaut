package com.ephox.argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._

object JsonTest extends Properties("Json") {
  property("One and only one is* satisfies (disjoint)") =
      forAll((j: Json) =>
        (List(j.isNull, j.isBool, j.isNumber, j.isString, j.isArray, j.isObject) filter (z => z) length) == 1)

  property("If is a number, then has a number value") =
      forAll((j: Json) =>
        j.number.isDefined == j.isNumber)

  property("If is a string, then has a string value") =
      forAll((j: Json) =>
        j.string.isDefined == j.isString)

  property("If is an array, then has an array value") =
      forAll((j: Json) =>
        j.array.isDefined == j.isArray)

  property("If is an object, then has an object value") =
      forAll((j: Json) =>
        j.objectt.isDefined == j.isObject)
}