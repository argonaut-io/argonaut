package com.ephox.argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Json._

object JsonTest extends Properties("Json") {
  property("One and only one is* satisfies (disjoint)") =
      forAll((j: Json) =>
        (List(j.isNull, j.isBool, j.isNumber, j.isString, j.isArray, j.isObject) filter (z => z) length) == 1)

  property("If is a boolean, then has a boolean value") =
      forAll((j: Json) =>
        j.ifBool(_ => true, false) == j.isBool)

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

  property("A boolean value isBool") =
      forAll((b: Boolean) =>
        jBool(b).isBool)

  property("A number value isNumber") =
      forAll((n: JsonNumber) =>
        jNumber(n).isNumber)

  property("A string value isString") =
      forAll((s: String) =>
        jString(s).isString)

  property("An array value isArray") =
      forAll((a: List[Json]) =>
        jArray(a).isArray)

  property("An object value isObject") =
      forAll((o: List[(String, Json)]) =>
        jObject(o).isObject)

  property("Prepending an object value results in a Json object") =
      forAll((v: (String, Json), j: Json) =>
        (v ->: j).objectt exists (_.length >= 1))

  property("Prepending an array value results in a Json object") =
      forAll((v: Json, j: Json) =>
        (v -->>: j).array exists (_.length >= 1))

  property("Prepending an object to a non-object value results in the same Json value") =
      forAll((v: (String, Json), j: Json) =>
        (!j.isObject) ==>
        ((v ~>: j) == j))

  property("Prepending an array to a non-array Json value results in the same Json value") =
      forAll((v: Json, j: Json) =>
        (!j.isArray) ==>
        ((v ~~>>: j) == j))
}