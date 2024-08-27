package argonaut

import Data.*
import Json.*
import scalaz.syntax.std.option.*

object JsonSpecification extends ArgonautSpec {

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
    jString isString                      $isString
    jArray isArray                        $isArray
    jSingleArray is single array          $isSingleArray
    jObject isObject                      $isObject
    jSingleObject is single object        $isSingleObject
    toString preserves order              $toStringPreservesOrder
  """

  def sameValue = prop((j: Json) => j == j)

  def modString = prop((j: JString) => j.withString(_ + "test") != j)

  def modNumber = prop((j: JNumber) =>
    j.withNumber { number =>
      JsonLong(number.toInt.map(n => if (n == 0) n + 1 else n * 2).getOrElse(0).toLong)
    } != j
  )

  def modArray = prop((j: JArray) => j.withArray(jEmptyArray :: _) != j)

  def modObject = prop((j: JObject) =>
    j.withObject(_ + ("veryunlikelytoberandomlygeneratedkey", jString("veryunlikelytoberandomlygeneratedvalue"))) != j
  )

  def modBoolean = prop((j: JBool) => j.not != j)

  def notComposeNot = prop((j: Json) => j.not.not == j)

  def noEffect = prop((j: Json) => (j.not == j) != j.isBool)

  def effectNotIsBool = prop((j: Json) => (j.not != j) == j.isBool)

  def effectWithNumber = prop((j: Json, k: JsonNumber => JsonNumber) => ((j withNumber k) == j) || j.isNumber)

  def effectWithString = prop((j: Json, k: JsonString => JsonString) => ((j withString k) == j) || j.isString)

  def effectWithArray = prop((j: Json, k: JsonArray => JsonArray) => ((j withArray k) == j) || j.isArray)

  def effectWithObject = prop((j: Json, k: JsonObject => JsonObject) => ((j withObject k) == j) || j.isObject)

  def arrayPrepend = prop((j: Json, e: Json) => !j.isArray || (e -->>: j).array.map(_.head) == e.some)

  def isBool = prop((b: Boolean) => jBool(b).isBool)

  def isString = prop((s: String) => jString(s).isString)

  def isArray = prop((a: JsonArray) => jArray(a).isArray)

  def isSingleArray = prop((j: Json) => jSingleArray(j).array == List(j).some)

  def isObject = prop((a: JsonObject) => jObject(a).isObject)

  def isSingleObject = prop((f: JsonField, j: Json) => (jSingleObject(f, j).obj map (_.toList)) == List((f, j)).some)

  def toStringPreservesOrder =
    prop((j: Json) => PrettyParams.nospace.copy(preserveOrder = true).pretty(j) === j.toString)
}
