package com.ephox
package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Json._

object JsonTest extends Properties("Json") {
  property("not compose not is id") =
      forAll((j: Json) =>
        j.not.not == j)

  property("no-effect not equals !isBool") =
      forAll((j: Json) =>
        (j.not == j) != j.isBool)

  property("effect not equals isBool") =
      forAll((j: Json) =>
        (j.not != j) == j.isBool)

  property("effect withNumber implies isNumber") =
      forAll((j: Json, k: JsonNumber => JsonNumber) =>
        ((j withNumber k) == j) || j.isNumber)

  property("effect withString implies isString") =
      forAll((j: Json, k: JsonString => JsonString) =>
        ((j withString k) == j) || j.isString)

  property("effect withArray implies isArray") =
      forAll((j: Json, k: JsonArray => JsonArray) =>
        ((j withArray k) == j) || j.isArray)

  property("effect withObject implies isObject") =
      forAll((j: Json, k: JsonObject => JsonObject) =>
        ((j withObject k) == j) || j.isObject)

  property("Object prepend puts element on head") =
      forAll((j: Json, e: JsonAssoc) =>
        !j.isObject || (e ->: j).objectt.map(_.head) == Some(e))






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
}