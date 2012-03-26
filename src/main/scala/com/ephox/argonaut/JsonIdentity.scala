package com.ephox
package argonaut

trait JsonIdentity[J] {
  val j: J
  
  import Json._
  import JsonLike._
  import JsonIdentity._

  /**
   * Return `true` if this JSON value is `null`, otherwise, `false`.
   */
  def isNull(implicit l: JsonLike[J]): Boolean =
    l.isNull(j)

  /**
   *  Returns the possible boolean of this JSON value.
   */
  def bool(implicit l: JsonLike[J]): Option[Boolean] =
    l.jBoolL.get(j)

  /**
   *  Returns the possible number of this JSON value.
   */
  def number(implicit l: JsonLike[J]): Option[JsonNumber] =
    l.jNumberL.get(j)

  /**
   * Returns the possible string of this JSON value.
   */
  def string(implicit l: JsonLike[J]): Option[JsonString] =
    l.jStringL.get(j)

  /**
   * Returns the possible array of this JSON value.
   */
  def array(implicit l: JsonLike[J]): Option[JsonArray] =
    l.jArrayL.get(j)

  /**
   * Returns the possible object of this JSON value.
   */
  def obj(implicit l: JsonLike[J]): Option[JsonObject] =
    l.jObjectL.get(j)

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
             
  /**
   * Alias for `field`.
   */
  def -|(f: => JsonField)(implicit l: JsonLike[J]): Option[Json] =
    field(f)

  /**
   * Returns a possible JSON value after traversing through JSON object values using the given field names.
   */
  def -||(fs: List[JsonField])(implicit l: JsonLike[J]): Option[Json] =
    fs match {
      case Nil => None
      case h::t => t.foldLeft(field(h))((a, b) => a flatMap (_ -| b))
    }

  /**
   * Return `true` if this JSON value is a boolean with a value of `true`, otherwise, `false`.
   */
  def isTrue(implicit l: JsonLike[J]): Boolean =
    bool exists (z => z)

  /**
   * Return `true` if this JSON value is a boolean with a value of `false`, otherwise, `false`.
   */
  def isFalse(implicit l: JsonLike[J]): Boolean =
    bool exists (z => !z)

  /**
   * Return `true` if this JSON value is a boolean.
   */
  def isBool(implicit l: JsonLike[J]): Boolean =
    isTrue || isFalse

  /**
   * Return `true` if this JSON value is a number.
   */
  def isNumber(implicit l: JsonLike[J]): Boolean =
    number.isDefined

  /**
   * Return `true` if this JSON value is a string.
   */
  def isString(implicit l: JsonLike[J]): Boolean =
    string.isDefined

  /**
   * Return `true` if this JSON value is a array.
   */
  def isArray(implicit l: JsonLike[J]): Boolean =
    array.isDefined

  /**
   * Return `true` if this JSON value is a object.
   */
  def isObject(implicit l: JsonLike[J]): Boolean =
    obj.isDefined

  /**
   * Returns the number of this JSON value, or the given default if this JSON value is not a number.
   *
   * @param d The default number if this JSON value is not a number.
   */
  def numberOr(d: => JsonNumber)(implicit l: JsonLike[J]): JsonNumber =
    number getOrElse d

  /**
   * Returns the string of this JSON value, or the given default if this JSON value is not a string.
   *
   * @param d The default string if this JSON value is not a string.
   */
  def stringOr(d: => JsonString)(implicit l: JsonLike[J]): JsonString =
    string getOrElse d

  /**
   * Returns the array of this JSON value, or the given default if this JSON value is not an array.
   *
   * @param d The default array if this JSON value is not an array.
   */
  def arrayOr(d: => JsonArray)(implicit l: JsonLike[J]): JsonArray =
    array getOrElse d

  /**
   * Returns the object of this JSON value, or the given default if this JSON value is not a object.
   *
   * @param d The default object if this JSON value is not an object.
   */
  def objectOr(d: => JsonObject)(implicit l: JsonLike[J]): JsonObject =
    obj getOrElse d

  /**
   * Returns this JSON number object or the value `0` if it is not a number.
   */
  def numberOrZero(implicit l: JsonLike[J]): JsonNumber =
    numberOr(0D)

  /**
   * Returns the string of this JSON value, or an empty string if this JSON value is not a string.
   */
  def stringOrEmpty(implicit l: JsonLike[J]): JsonString =
    stringOr("")

  /**
   * Returns the array of this JSON value, or an empty array if this JSON value is not an array.
   */
  def arrayOrEmpty(implicit l: JsonLike[J]): JsonArray =
    arrayOr(Nil)

  /**
   * Returns the object of this JSON value, or the empty object if this JSON value is not an object.
   */
  def objectOrEmpty(implicit l: JsonLike[J]): JsonObject =
    objectOr(Nil)

  /**
   * Returns the possible object map of this JSON value.
   */
  def objectMap(implicit l: JsonLike[J]): Option[JsonObjectMap] =
    obj map (_.toMap)

  /**
   * Returns the object map of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param m The default object map if this JSON value is not an object.
   */
  def objectMapOr(m: => JsonObjectMap)(implicit l: JsonLike[J]): JsonObjectMap =
    objectMap getOrElse m

  /**
   * Returns the object map of this JSON value, or the empty map if this JSON value is not an object.
   */
  def objectMapOrEmpty(implicit l: JsonLike[J]): JsonObjectMap =
    objectMapOr(Map.empty)

  /**
   * Return the object keys if this JSON value is an object, otherwise, return the empty list.
   */
  def objectFields(implicit l: JsonLike[J]): Option[Iterable[JsonField]] =
    objectMap map (_.keys)

  /**
   * Returns the object map keys of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param f The default object map keys if this JSON value is not an object.
   */
  def objectFieldsOr(f: => Iterable[JsonField])(implicit l: JsonLike[J]): Iterable[JsonField] =
    objectFields getOrElse f

  /**
   * Returns the object map keys of this JSON value, or the empty list if this JSON value is not an object.
   */
  def objectFieldsOrEmpty(implicit l: JsonLike[J]): Iterable[JsonField] =
    objectFieldsOr(Iterable.empty)

  /**
   * Return the object values if this JSON value is an object, otherwise, return the empty list.
   */
  def objectValues(implicit l: JsonLike[J]): Option[Iterable[Json]] =
    objectMap map (_.values)

  /**
   * Returns the object map values of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param k The default object map values if this JSON value is not an object.
   */
  def objectValuesOr(k: => Iterable[Json])(implicit l: JsonLike[J]): Iterable[Json] =
    objectValues getOrElse k

  /**
   * Returns the object map values of this JSON value, or the empty list if this JSON value is not an object.
   */
  def objectValuesOrEmpty(implicit l: JsonLike[J]): Iterable[Json] =
    objectValuesOr(Iterable.empty)

  /**
   * Returns `true` if this is a JSON object which has the given field, `false` otherwise.
   */
  def hasField(f: => JsonField)(implicit l: JsonLike[J]): Boolean =
    objectMap exists (_ isDefinedAt f)

  /**
   * Returns the possible value for the given JSON object field.
   */
  def field(f: => JsonField)(implicit l: JsonLike[J]): Option[Json] =
    objectMap flatMap (_ get f)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns the default..
   */
  def fieldOr(f: => JsonField, j: => Json)(implicit l: JsonLike[J]): Json =
    field(f) getOrElse j

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON `null`..
   */
  def fieldOrNull(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jNull[Json])

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON boolean with the value `true`.
   */
  def fieldOrTrue(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jTrue[Json])

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON boolean with the value `false`.
   */
  def fieldOrFalse(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jFalse[Json])

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON number with the value `0`.
   */
  def fieldOrZero(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jZero[Json])

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns an empty JSON.
   */
  def fieldOrEmptyString(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jEmptyString[Json])

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns an empty JSON array.
   */
  def fieldOrEmptyArray(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jEmptyArray[Json])

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns an empty JSON object.
   */
  def fieldOrEmptyObject(f: => JsonField)(implicit l: JsonLike[J]): Json =
    fieldOr(f, jEmptyObject[Json])

}

object JsonIdentity extends JsonIdentitys

trait JsonIdentitys {
  implicit def ToJsonIdentity[J](k: J): JsonIdentity[J] =
    new JsonIdentity[J] {
      val j = k  
    }
  
  implicit def FromJsonIdentity[J](k: JsonIdentity[J]): J =
    k.j
}