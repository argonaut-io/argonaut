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
  def is = "Json" ^
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
    "effect withArray implies isArray" ! prop((j: Json, k: JsonArray => JsonArray) =>
      ((j withArray k) === j) || j.isArray) ^
    "effect withObject implies isObject" ! prop((j: Json, k: JsonObject => JsonObject) =>
      ((j withObject k) === j) || j.isObject) ^
    "Array prepend puts element on head" ! prop((j: Json, e: Json) =>
      !j.isArray || (e -->>: j).array.map(_.head) === e.some) ^
    "jBool isBool" ! prop((b: Boolean) =>
      jBool(b).isBool) ^
    "jNumber isNumber" ! prop((n: JsonNumber) =>
      jNumber(n).isNumber) ^
    "jString isString" ! prop((s: String) =>
      jString(s).isString) ^
    "jArray isArray" ! prop((a: JsonArray) =>
      jArray(a).isArray) ^
    "jSingleArray is single array" ! prop((j: Json) =>
      jSingleArray(j).array === List(j).some) ^
    "jObject isObject" ! prop((a: JsonObject) =>
      jObject(a).isObject) ^
    "jSingleObject is single object" ! prop((f: JsonField, j: Json) =>
      (jSingleObject(f, j).obj map (_.toList)) === List((f, j)).some) ^ end
}
