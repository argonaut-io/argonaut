package com.ephox
package argonaut

/**
 * A data type representing possible <a href="http://www.json.org/">JSON</a> values.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
sealed trait Json {
  import Json._

  /**
   * The catamorphism for the JSON value data type.
   */
  def fold[X](
    jsonNull: => X,
    jsonBool: Boolean => X,
    jsonNumber: JsonNumber => X,
    jsonString: String => X,
    jsonArray: JsonArray => X,
    jsonObject: JsonObject => X
  ): X

  private val p = PossibleJson.pJson(this)

  /**
   * Compare two JSON values for equality.
   */
  override def equals(o: Any) =
    o.isInstanceOf[Json] && o.asInstanceOf[Json].fold(
      p.isNull,
      b => p.bool.exists(_ == b),
      n => p.number exists (_ == n),
      s => p.string exists (_ == s),
      a => p.array exists (_ == a),
      o => p.obj exists (_ == o)
    )

  /**
   * Compute a hash-code for this JSON value.
   */
  override def hashCode =
    fold(
      0,
      _.hashCode,
      _.hashCode,
      _.hashCode,
      _.hashCode,
      _.hashCode
    )

  /**
   * Compute a `String` representation for this JSON value.
   */
  override def toString =
    "Json{" +
        fold(
          "null",
          "bool[" + _ + "]",
          "number[" + _ + "]",
          "string[" + _ + "]",
          "array[" + _ + "]",
          "object[" + _ + "]"
        ) + "}"
}

object Json extends Jsons

/**
 * Constructors and other utilities for JSON values.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
trait Jsons {
  type JsonNumber = Double
  type JsonArray = List[Json]
  type JsonString = String
  type JsonField = String
  type JsonAssoc = (JsonField, Json)
  type JsonObject = List[JsonAssoc]
  type JsonObjectMap = Map[JsonField, Json]

  /**
   * Construct a JSON value that is `null`.
   */
  private[argonaut] val jjNull: Json = new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jnull
  }

  /**
   * Construct a JSON value that is a boolean.
   */
  private[argonaut] val jjBool: Boolean => Json = b => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jbool(b)
  }

  /**
   * Construct a JSON value that is a number.
   */
  private[argonaut] val jjNumber: JsonNumber => Json = n => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jnumber(n)
  }

  /**
   * Construct a JSON value that is a string.
   */
  private[argonaut] val jjString: String => Json = s => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jstring(s)
  }

  /**
   * Construct a JSON value that is an array.
   */
  private[argonaut] val jjArray: JsonArray => Json = a => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jarray(a)
  }

  /**
   * Construct a JSON value that is an object.
   */
  private[argonaut] val jjObject: JsonObject => Json = x => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jobject(x)
  }


  // todo delete all below

  /**
   * Construct a JSON value that is `null`.
   */
  val jNull: Json = new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jnull
  }

  /**
   * Construct a JSON value that is a boolean.
   */
  val jBool: Boolean => Json = b => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jbool(b)
  }

  /**
   * Construct a JSON boolean value of `true`.
   */
  val jTrue = jBool(true)

  /**
   * Construct a JSON boolean value of `false`.
   */
  val jFalse = jBool(false)

  /**
   * Construct a JSON value that is a number.
   */
  val jNumber: JsonNumber => Json = n => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jnumber(n)
  }

  /**
   * A JSON value that is a zero number.
   */
  val jZero = jNumber(0D)

  /**
   * Construct a JSON value that is a string.
   */
  val jString: String => Json = s => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jstring(s)
  }

  /**
   * A JSON value that is an empty string.
   */
  val jEmptyString = jString("")

  /**
   * Construct a JSON value that is an array.
   */
  val jArray: JsonArray => Json = a => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jarray(a)
  }

  /**
   * A JSON value that is an empty array.
   */
  val jEmptyArray = jArray(Nil)

  /**
   * Returns a function that takes a single value and produces a JSON array that contains only that value.
   */
  val jSingleArray: Json => Json = j => jArray(List(j))

  /**
   * Construct a JSON value that is an object.
   */
  val jObject: JsonObject => Json = x => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jobject(x)
  }

  /**
   * A JSON value that is an empty object.
   */
  val jEmptyObject = jObject(Nil)

  /**
   * Returns a function that takes an association value and produces a JSON object that contains only that value.
   */
  val jSingleObject: JsonField => Json => Json = k => v => jObject(List((k, v)))

  /**
   * Construct a JSON value that is an object from an index.
   */
  val jObjectMap = (x: JsonObjectMap) => jObject(x.toList)

  import scalaz._, PLens._, CoStateT._

  implicit def JsonJsonLike: JsonLike[Json] =
    new JsonLike[Json] {
      def jBoolL: Json @-? Boolean =
        PLens(_.fold(None, z => Some(coState(jjBool, z)), _ => None, _ => None, _ => None, _ => None))

      def jNumberL: Json @-? JsonNumber =
        PLens(_.fold(None, _ => None, z => Some(coState(jjNumber, z)), _ => None, _ => None, _ => None))

      def jStringL: Json @-? JsonString =
        PLens(_.fold(None, _ => None, _ => None, z => Some(coState(jjString, z)), _ => None, _ => None))

      def jArrayL: Json @-? JsonArray =
        PLens(_.fold(None, _ => None, _ => None, _ => None, z => Some(coState(jjArray, z)), _ => None))

      def jObjectL: Json @-? JsonObject =
        PLens(_.fold(None, _ => None, _ => None, _ => None, _ => None, z => Some(coState(jjObject, z))))

      def jNull =
        jjNull

      def jBool =
        jjBool

      def jNumber =
        jjNumber

      def jString =
        jjString

      def jArray =
        jjArray

      def jObject =
        jjObject
    }

  /**
   * Implicitly convert to a possible JSON.
   */
  implicit def JsonPossible(j: Json): PossibleJson = PossibleJson.pJson(j)
}
