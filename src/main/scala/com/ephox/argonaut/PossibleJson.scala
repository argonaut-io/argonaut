package com.ephox
package argonaut

/**
 * A data type that represents either a [[com.ephox.argonaut.Json]] or no value.
 * This is isomorphic to `Option[Json]`.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
sealed trait PossibleJson {
  import Json._
  import PossibleJson._

  /**
   * The catamorphism for the possible JSON value data type.
   */
  def fold[X](
    jsonNull: => X,
    jsonBool: Boolean => X,
    jsonNumber: JsonNumber => X,
    jsonString: String => X,
    jsonArray: JsonArray => X,
    jsonObject: JsonObject => X,
    none: => X
  ): X

  /**
   * Run the first function if this is not empty (has a JSON value), otherwise, return the other value.
   */
  def json[X](is: Json => X, isnt: => X) =
    fold(is(jNull), x => is(jBool(x)), x => is(jNumber(x)), x => is(jString(x)), x => is(jArray(x)), x => is(jObject(x)), isnt)

  /**
   * Return the Json if this is not empty, or the given value.
   */
  def jsonOr(j: => Json) =
    json(x => x, j)

  /**
   *  Returns the possible boolean of this JSON value.
   */
  def bool: Option[Boolean] =
    fold(None, Some(_), _ => None, _ => None, _ => None, _ => None, None)

  /**
   *  Returns the possible number of this JSON value.
   */
  def number: Option[JsonNumber] =
    fold(None, _ => None, Some(_), _ => None, _ => None, _ => None, None)

  /**
   * Returns the possible string of this JSON value.
   */
  def string: Option[JsonString] =
    fold(None, _ => None, _ => None, Some(_), _ => None, _ => None, None)

  /**
   * Returns the possible array of this JSON value.
   */
  def array: Option[JsonArray] =
    fold(None, _ => None, _ => None, _ => None, Some(_), _ => None, None)

  /**
   * Returns the possible object of this JSON value.
   */
  def obj: Option[JsonObject] =
    fold(None, _ => None, _ => None, _ => None, _ => None, Some(_), None)

  /**
   * Returns true if this possible JSON value is empty (has no JSON value).
   */
  def isEmpty: Boolean =
    fold(false, _ => false, _ => false, _ => false, _ => false, _ => false, true)

  /**
   * Return `true` if this JSON value is `null`, otherwise, `false`.
   */
  def isNull: Boolean =
    fold(true, _ => false, _ => false, _ => false, _ => false, _ => false, false)

  /**
   * Return `true` if this JSON value is a boolean with a value of `true`, otherwise, `false`.
   */
  def isTrue: Boolean =
    fold(false, x => x, _ => false, _ => false, _ => false, _ => false, false)

  /**
   * Return `true` if this JSON value is a boolean with a value of `false`, otherwise, `false`.
   */
  def isFalse: Boolean =
    fold(false, x => !x, _ => false, _ => false, _ => false, _ => false, false)

  /**
   * Return `true` if this JSON value is a boolean.
   */
  def isBool = isTrue || isFalse

  /**
   * Return `true` if this JSON value is a number.
   */
  def isNumber = number.isDefined

  /**
   * Return `true` if this JSON value is a string.
   */
  def isString = string.isDefined

  /**
   * Return `true` if this JSON value is a array.
   */
  def isArray = array.isDefined

  /**
   * Return `true` if this JSON value is a object.
   */
  def isObject = obj.isDefined

  /**
   * Returns the number of this JSON value, or the given default if this JSON value is not a number.
   *
   * @param d The default number if this JSON value is not a number.
   */
  def numberOr(d: => JsonNumber) = number getOrElse d

  /**
   * Returns the string of this JSON value, or the given default if this JSON value is not a string.
   *
   * @param s The default string if this JSON value is not a string.
   */
  def stringOr(d: => JsonString) = string getOrElse d

  /**
   * Returns the array of this JSON value, or the given default if this JSON value is not an array.
   *
   * @param a The default array if this JSON value is not an array.
   */
  def arrayOr(d: => JsonArray) = array getOrElse d

  /**
   * Returns the object of this JSON value, or the given default if this JSON value is not a object.
   *
   * @param o The default object if this JSON value is not an object.
   */
  def objectOr(d: => JsonObject) = obj getOrElse d

  /**
   * Returns this JSON number object or the value `0` if it is not a number.
   */
  def numberOrZero = numberOr(0D)

  /**
   * Returns the string of this JSON value, or an empty string if this JSON value is not a string.
   */
  def stringOrEmpty = stringOr("")

  /**
   * Returns the array of this JSON value, or an empty array if this JSON value is not an array.
   */
  def arrayOrEmpty = arrayOr(Nil)

  /**
   * Returns the object of this JSON value, or the empty object if this JSON value is not an object.
   */
  def objectOrEmpty = objectOr(Nil)

  /**
   * If this JSON value is a number, run the given function on it, otherwise, leave this value unchanged.
   *
   * @param The function to run if this JSON value is a number.
   */
  def usingNumber[X](k: JsonNumber => X, z: => X) =
    number match {
      case Some(a) => k(a)
      case None    => z
    }

  /**
   * If this JSON value is a string, run the given function on it, otherwise, leave this value unchanged.
   *
   * @param The function to run if this JSON value is a string.
   */
  def usingString[X](k: JsonString => X, z: => X) =
    string match {
      case Some(a) => k(a)
      case None    => z
    }

  /**
   * If this JSON value is an array, run the given function on it, otherwise, leave this value unchanged.
   *
   * @param The function to run if this JSON value is an array.
   */
  def usingArray[X](k: JsonArray => X, z: => X) =
    array match {
      case Some(a) => k(a)
      case None    => z
    }

  /**
   * If this JSON value is an object, run the given function on it, otherwise, leave this value unchanged.
   *
   * @param The function to run if this JSON value is an object.
   */
  def usingObject[X](k: JsonObject => X, z: => X) =
    obj match {
      case Some(a) => k(a)
      case None    => z
    }

  /**
   * Returns the possible object map of this JSON value.
   */
  lazy val objectMap: Option[JsonObjectMap] = obj map (_.toMap)

  /**
   * Returns the object map of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param m The default object map if this JSON value is not an object.
   */
  def objectMapOr(m: => JsonObjectMap) = objectMap getOrElse m

  /**
   * Returns the object map of this JSON value, or the empty map if this JSON value is not an object.
   */
  def objectMapOrEmpty = objectMapOr(Map.empty)

  /**
   * Return the object keys if this JSON value is an object, otherwise, return the empty list.
   */
  def objectFields = objectMap map (_.keys)

  /**
   * Returns the object map keys of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param k The default object map keys if this JSON value is not an object.
   */
  def objectFieldsOr(f: => Iterable[JsonField]): Iterable[JsonField] = objectFields getOrElse f

  /**
   * Returns the object map keys of this JSON value, or the empty list if this JSON value is not an object.
   */
  def objectFieldsOrEmpty = objectFieldsOr(Iterable.empty)

  /**
   * Return the object values if this JSON value is an object, otherwise, return the empty list.
   */
  def objectValues = objectMap map (_.values)

  /**
   * Returns the object map values of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param k The default object map values if this JSON value is not an object.
   */
  def objectValuesOr(k: => Iterable[Json]): Iterable[Json] = objectValues getOrElse k

  /**
   * Returns the object map values of this JSON value, or the empty list if this JSON value is not an object.
   */
  def objectValuesOrEmpty = objectValuesOr(Iterable.empty)

  /**
   * Returns `true` if this is a JSON object whcih has the given field, `false` otherwise.
   */
  def hasField(f: => JsonField) = objectMap exists (_ isDefinedAt f)

  /**
   * Returns the possible value for the given JSON object field.
   */
  def field(f: => JsonField) = objectMap flatMap (_ get f)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns the default..
   */
  def fieldOr(f: => JsonField, j: => Json) = field(f) getOrElse j

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON `null`..
   */
  def fieldOrNull(f: => JsonField) = fieldOr(f, jNull)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON boolean with the value `true`.
   */
  def fieldOrTrue(f: => JsonField) = fieldOr(f, jTrue)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON boolean with the value `false`.
   */
  def fieldOrFalse(f: => JsonField) = fieldOr(f, jFalse)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns a JSON number with the value `0`.
   */
  def fieldOrZero(f: => JsonField) = fieldOr(f, jZero)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns an empty JSON.
   */
  def fieldOrEmptyString(f: => JsonField) = fieldOr(f, jEmptyString)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns an empty JSON array.
   */
  def fieldOrEmptyArray(f: => JsonField) = fieldOr(f, jEmptyArray)

  /**
   * Returns the value for the given JSON object field if this is an object with the given field, otherwise, returns an empty JSON object.
   */
  def fieldOrEmptyObject(f: => JsonField) = fieldOr(f, jEmptyObject)

  /**
   * Folds-right the given accumulator function and element over this possible JSON array value.
   */
  def foldRightArray[B](b: => B): B => ((Json, B) => B) => B =
    z => f => array match {
      case None => b
      case Some(a) => a.foldRight(z)(f)
    }

  /**
   * Folds-right the given accumulator function and element over this possible JSON object value.
   */
  def foldRightObject[B](b: => B): B => ((JsonAssoc, B) => B) => B =
    z => f => obj match {
      case None => b
      case Some(o) => o.foldRight(z)(f)
    }

  /**
   * Folds-right the given accumulator function and element over this possible JSON array value.
   */
  def foldLeftArray[B](b: => B): B => ((B, Json) => B) => B =
    z => f => array match {
      case None => b
      case Some(a) => a.foldLeft(z)(f)
    }

  /**
   * Folds-right the given accumulator function and element over this possible JSON object value.
   */
  def foldLeftObject[B](b: => B): B => ((B, JsonAssoc) => B) => B =
    z => f => obj match {
      case None => b
      case Some(o) => o.foldLeft(z)(f)
    }

  /**
   * Returns the number of elements in the JSON array, or the default if this is not a JSON array.
   */
  def lengthArray(n: => Int): Int =
    foldLeftArray[Int](n)(0)((x, _) => x + 1)


  /**
   * Returns the number of elements in the JSON array, or the default if this is not a JSON array.
   */
  def lengthObject(n: => Int): Int =
    foldLeftObject[Int](n)(0)((x, _) => x + 1)

  /**
   * If this is a JSON boolean value, invert the `true` and `false` values, otherwise, leave unchanged.
   */
  def not =
    if(isTrue) pJson(jFalse)
    else if(isFalse) pJson(jTrue)
    else this

  /**
   * If this is a JSON number value, run the given function on the value, otherwise, leave unchanged.
   */
  val withNumber: (JsonNumber => JsonNumber) => PossibleJson =
    k => number match {
      case Some(d) => pJson(jNumber(k(d)))
      case None => this
    }

  /**
   * If this is a JSON string value, run the given function on the value, otherwise, leave unchanged.
   */
  val withString: (JsonString => JsonString) => PossibleJson =
    k => string match {
      case Some(s) => pJson(jString(k(s)))
      case None => this
    }

  /**
   * If this is a JSON array value, run the given function on the value, otherwise, leave unchanged.
   */
  val withArray: (JsonArray => JsonArray) => PossibleJson =
    k => array match {
      case Some(a) => pJson(jArray(k(a)))
      case None => this
    }

  /**
   * If this is a JSON object value, run the given function on the value, otherwise, leave unchanged.
   */
  val withObject: (JsonObject => JsonObject) => PossibleJson =
    k => obj match {
      case Some(o) => pJson(jObject(k(o)))
      case None => this
    }

  /**
   * If this is a JSON number value, run the given functions on the value, otherwise, leave unchanged.
   */
  def withsNumber(ns: (JsonNumber => JsonNumber)*): PossibleJson =
    withNumber(ns.foldRight[JsonNumber => JsonNumber](x => x)(_ compose _))

  /**
   * If this is a JSON string value, run the given functions on the value, otherwise, leave unchanged.
   */
  def withsString(ns: (JsonString => JsonString)*): PossibleJson =
    withString(ns.foldRight[JsonString => JsonString](x => x)(_ compose _))

  /**
   * If this is a JSON array value, run the given functions on the value, otherwise, leave unchanged.
   */
  def withsArray(ns: (JsonArray => JsonArray)*): PossibleJson =
    withArray(ns.foldRight[JsonArray => JsonArray](x => x)(_ compose _))

  /**
   * If this is a JSON array value, run the given functions on the value, otherwise, leave unchanged.
   */
  def withsObject(ns: (JsonObject => JsonObject)*): PossibleJson =
    withObject(ns.foldRight[JsonObject => JsonObject](x => x)(_ compose _))

  /**
   * If this is a JSON object, then prepend the given value, otherwise, return a JSON object with only the given value.
   */
  def ->:(j: => JsonAssoc) = withObject(j :: _)

  /**
   * If this is a JSON array, then prepend the given value, otherwise, return a JSON array with only the given value.
   */
  def -->>:(j: => Json) = withArray(j :: _)

  /**
   * Returns a possible JSON value for the given field. If this JSON value is not an object or does not have a value
   * associated with the given field, then an empty possible JSON is returned.
   */
  def -|(f: => JsonField): PossibleJson = field(f) match {
    case Some(a) => pJson(a)
    case None    => eJson
  }

  /**
   * Returns a possible JSON value after traversing through JSON object values using the given field names.
   */
  def -||(fs: => Iterable[JsonField]) = fs.foldLeft(this)(_ -| _)

  /**
   * Compare two possible JSON values for equality.
   */
  override def equals(o: Any) =
    o.isInstanceOf[PossibleJson] && o.asInstanceOf[PossibleJson].fold(
      isNull,
      b => bool.exists(_ == b),
      n => number exists (_ == n),
      s => string exists (_ == s),
      a => array exists (_ == a),
      o => obj exists (_ == o),
      isEmpty
    )

  /**
   * Compute a hash-code for this possible JSON value.
   */
  override def hashCode =
    fold(
      0,
      _.hashCode,
      _.hashCode,
      _.hashCode,
      _.hashCode,
      _.hashCode,
      1
    )

  /**
   * Compute a `String` representation for this possible JSON value.
   */
  override def toString =
    "PossibleJson{" +
        fold(
          "null",
          "bool[" + _ + "]",
          "number[" + _ + "]",
          "string[" + _ + "]",
          "array[" + _ + "]",
          "object[" + _ + "]",
          "empty"
        ) + " }"

}

object PossibleJson extends PossibleJsons

/**
 * Constructors and other utilities for possible JSON values.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
trait PossibleJsons {
  import Json._

  /**
   * An empty possible JSON value.
   */
  val eJson: PossibleJson = new PossibleJson {
    def fold[X](
      jsonNull: => X,
      jsonBool: Boolean => X,
      jsonNumber: JsonNumber => X,
      jsonString: String => X,
      jsonArray: JsonArray => X,
      jsonObject: JsonObject => X,
      none: => X
    ) = none
  }

  /**
   * A function that constructs a possible JSON value with the given JSON value.
   */
  val pJson: Json => PossibleJson = k => new PossibleJson {
    def fold[X](
      jsonNull: => X,
      jsonBool: Boolean => X,
      jsonNumber: JsonNumber => X,
      jsonString: String => X,
      jsonArray: JsonArray => X,
      jsonObject: JsonObject => X,
      none: => X
    ) = k.fold(jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject)
  }

  import scalaz._, PLens._, CoStateT._

  implicit def PossibleJsonJsonLike: JsonLike[PossibleJson] =
    new JsonLike[PossibleJson] {
      def jBoolL: PossibleJson @-? Boolean =
        PLens(_.bool map (coState(jBool, _)))

      def jNumberL: PossibleJson @-? JsonNumber =
        PLens(_.number map (coState(jNumber, _)))

      def jStringL: PossibleJson @-? JsonString =
        PLens(_.string map (coState(jString, _)))

      def jArrayL: PossibleJson @-? JsonArray =
        PLens(_.array map (coState(jArray, _)))

      def jObjectL: PossibleJson @-? JsonObject =
        PLens(_.obj map (coState(jObject, _)))

      def jNull =
        pJson(jjNull)

      def jBool =
        pJson compose jjBool

      def jNumber =
        pJson compose jjNumber

      def jString =
        pJson compose jjString

      def jArray =
        pJson compose jjArray

      def jObject =
        pJson compose jjObject
    }

}
