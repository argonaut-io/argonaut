package com.ephox
package argonaut

trait JsonIdentity[J] {
  val j: J
  
  import Json._
  import JsonLike._

  /**
   * If this is a JSON boolean value, invert the `true` and `false` values, otherwise, leave unchanged.
   */
  def not(implicit l: JsonLike[J]): J =
    jBoolL[J] mod (!_, j)
  
  /**
   * If this is a JSON number value, run the given function on the value, otherwise, leave unchanged.
   */
  def withNumber(k: JsonNumber => JsonNumber)(implicit l: JsonLike[J]): J =
    jNumberL[J] mod (k, j)

  /**
   * If this is a JSON string value, run the given function on the value, otherwise, leave unchanged.
   */
  def withString(k: JsonString => JsonString)(implicit l: JsonLike[J]): J =
    jStringL[J] mod (k, j)

  /**
   * If this is a JSON array value, run the given function on the value, otherwise, leave unchanged.
   */
  def withArray(k: JsonArray => JsonArray)(implicit l: JsonLike[J]): J =
    jArrayL[J] mod (k, j)

  /**
   * If this is a JSON object value, run the given function on the value, otherwise, leave unchanged.
   */
  def withObject(k: JsonObject => JsonObject)(implicit l: JsonLike[J]): J =
    jObjectL[J] mod (k, j)

  /**
   * If this is a JSON object, then prepend the given value, otherwise, return a JSON object with only the given value.
   */
  def ->:(k: => JsonAssoc)(implicit l: JsonLike[J]): J =
    withObject(k :: _)

  /**
   * If this is a JSON object, and the association is set, then prepend the given value, otherwise, return a JSON object with only the given value.
   */
  def ->?:(o: => Option[JsonAssoc])(implicit l: JsonLike[J]): J =
    o.map(k => withObject(k :: _)).getOrElse(j)

  /**
   * If this is a JSON array, then prepend the given value, otherwise, return a JSON array with only the given value.
   */
  def -->>:(k: => Json)(implicit l: JsonLike[J]): J =
    withArray(k :: _)

  /**
   * If this is a JSON array, and the element is set, then prepend the given value, otherwise, return a JSON array with only the given value.
   */
  def -->>?:(o: => Option[Json])(implicit l: JsonLike[J]): J =
    o.map(j => withArray(j :: _)).getOrElse(j)

}

object JsonIdentity extends JsonIdentitys

trait JsonIdentitys {
  def ToJsonIdentity[J](k: J): JsonIdentity[J] =
    new JsonIdentity[J] {
      val j = k  
    }
  
  def FromJsonIdentity[J](k: JsonIdentity[J]): J =
    k.j
}