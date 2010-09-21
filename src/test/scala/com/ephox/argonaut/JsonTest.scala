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
    
}