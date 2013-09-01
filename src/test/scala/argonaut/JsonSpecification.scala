package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Json._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

object JsonSpecification extends Specification with ScalaCheck {
  // NOTE: List[Json] should be JsonArray, but it is failing to resolve under 2.10.0 with type alias.

  def is = "Json" ^
    "same value should be equal" ! prop((j: Json) =>
      j === j) ^
    "modified string should not be equal" ! prop((j: JString) =>
      j.withString(_ + "test") /== j) ^
    "modified number should not be equal" ! prop((j: JNumber) =>
      j.withNumber(number => if (number === 0.0d) number + 1 else number * 2) /== j) ^
    "modified array should not be equal" ! prop((j: JArray) =>
      j.withArray(jEmptyArray :: _) /== j) ^
    "modified object should not be equal" ! prop((j: JObject) =>
      j.withObject(_ + ("veryunlikelytoberandomlygeneratedkey", jString("veryunlikelytoberandomlygeneratedvalue"))) /== j) ^
    "modified boolean should not be equal" ! prop((j: JBool) =>
      j.not /== j) ^
    "not compose not is id" ! prop((j: Json) =>
      j.not.not === j) ^
    "no-effect not equals !isBool" ! prop((j: Json) =>
      (j.not === j) !== j.isBool) ^
    "effect not equals isBool" ! prop((j: Json) =>
      (j.not /== j) === j.isBool) ^
    "effect withNumber implies isNumber" ! prop((j: Json, k: JsonNumber => JsonNumber) =>
      ((j withNumber k) === j) || j.isNumber) ^
    "effect withString implies isString" ! prop((j: Json, k: JsonString => JsonString) =>
      ((j withString k) === j) || j.isString) ^
    "effect withArray implies isArray" ! prop((j: Json, k: List[Json] => List[Json]) =>
      ((j withArray k) === j) || j.isArray) ^
    "effect withObject implies isObject" ! prop((j: Json, k: JsonObject => JsonObject) =>
      ((j withObject k) === j) || j.isObject) ^
    "Array prepend puts element on head" ! prop((j: Json, e: Json) =>
      !j.isArray || (e -->>: j).array.map(_.head) === e.some) ^
    "jBool isBool" ! prop((b: Boolean) =>
      jBool(b).isBool) ^
    "jNumber isNumber" ! prop((n: JsonNumber) => !n.isNaN && !n.isInfinity ==>
      jNumberOrNull(n).isNumber) ^
    "jString isString" ! prop((s: String) =>
      jString(s).isString) ^
    "jArray isArray" ! prop((a: List[Json]) =>
      jArray(a).isArray) ^
    "jSingleArray is single array" ! prop((j: Json) =>
      jSingleArray(j).array === List(j).some) ^
    "jObject isObject" ! prop((a: JsonObject) =>
      jObject(a).isObject) ^
    "jSingleObject is single object" ! prop((f: JsonField, j: Json) =>
      (jSingleObject(f, j).obj map (_.toList)) === List((f, j)).some) ^ end
}
