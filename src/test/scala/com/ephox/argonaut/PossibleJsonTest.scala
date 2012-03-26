package com.ephox
package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Json._
import JsonIdentity._
import JsonLike._

object PossibleJsonTest extends Properties("PossibleJson") {

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
        j.obj.isDefined == j.isObject)

  property("A boolean value isBool") =
      forAll((b: Boolean) =>
        jBool[PossibleJson](b).isBool)

  property("A number value isNumber") =
      forAll((n: JsonNumber) =>
        jNumber[PossibleJson](n).isNumber)

  property("A string value isString") =
      forAll((s: String) =>
        jString[PossibleJson](s).isString)

  property("An array value isArray") =
      forAll((a: List[Json]) =>
        jArray[PossibleJson](a).isArray)

  property("An object value isObject") =
      forAll((o: List[(String, Json)]) =>
        jObject[PossibleJson](o).isObject)

}
