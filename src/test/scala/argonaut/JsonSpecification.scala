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

  def is = s2"""
  Json
    same value should be equal            $sameValue
    modified string should not be equal   $modString
    modified number should not be equal   $modNumber
    modified array should not be equal    $modArray
    modified object should not be equal   $modObject
    modified boolean should not be equal  $modBoolean
    not compose not is id                 $notComposeNot
    no-effect not equals !isBool          $noEffect
    effect not equals isBool              $effectNotIsBool
    effect withNumber implies isNumber    $effectWithNumber
    effect withString implies isString    $effectWithString
    effect withArray implies isArray      $effectWithArray
    effect withObject implies isObject    $effectWithObject
    Array prepend puts element on head    $arrayPrepend
    jBool isBool                          $isBool
    jNumber isNumber                      $isNumber
    jString isString                      $isString
    jArray isArray                        $isArray
    jSingleArray is single array          $isSingleArray
    jObject isObject                      $isObject
    jSingleObject is single object        $isSingleObject
  """

  def sameValue = prop((j: Json) => j === j)

  def modString = prop((j: JString) => j.withString(_ + "test") /== j)

  def modNumber = prop((j: JNumber) => j.withNumber { number =>
    JsonLong(number.toInt.map(n =>
      if (n === 0) (n + 1) else (n * 2)
    ).getOrElse(0).toLong)
  } /== j)

  def modArray = prop((j: JArray) => j.withArray(jEmptyArray :: _) /== j)

  def modObject = prop((j: JObject) => j.withObject(_ + ("veryunlikelytoberandomlygeneratedkey", jString("veryunlikelytoberandomlygeneratedvalue"))) /== j)

  def modBoolean = prop((j: JBool) => j.not /== j)

  def notComposeNot = prop((j: Json) => j.not.not === j)

  def noEffect = prop((j: Json) => (j.not === j) !== j.isBool)

  def effectNotIsBool = prop((j: Json) => (j.not /== j) === j.isBool)

  def effectWithNumber = prop((j: Json, k: JsonNumber => JsonNumber) => ((j withNumber k) === j) || j.isNumber)

  def effectWithString = prop((j: Json, k: JsonString => JsonString) => ((j withString k) === j) || j.isString)

  def effectWithArray = prop((j: Json, k: List[Json] => List[Json]) => ((j withArray k) === j) || j.isArray)

  def effectWithObject = prop((j: Json, k: JsonObject => JsonObject) => ((j withObject k) === j) || j.isObject)

  def arrayPrepend = prop((j: Json, e: Json) => !j.isArray || (e -->>: j).array.map(_.head) === e.some)

  def isBool = prop((b: Boolean) => jBool(b).isBool)

  def isNumber = prop((n: JsonNumber) => !n.isNaN && !n.isInfinity ==> n.asJsonOrNull.isNumber)

  def isString = prop((s: String) => jString(s).isString)

  def isArray = prop((a: List[Json]) => jArray(a).isArray)

  def isSingleArray = prop((j: Json) => jSingleArray(j).array === List(j).some)

  def isObject = prop((a: JsonObject) => jObject(a).isObject)

  def isSingleObject = prop((f: JsonField, j: Json) => (jSingleObject(f, j).obj map (_.toList)) === List((f, j)).some)
}
