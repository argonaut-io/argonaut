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
  import JsonIdentity._

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

  /**
   * Compare two JSON values for equality.
   */
  override def equals(o: Any) =
    o.isInstanceOf[Json] && o.asInstanceOf[Json].fold(
      this.isNull,
      b => this.bool exists (_ == b),
      n => this.number exists (_ == n),
      s => this.string exists (_ == s),
      a => this.array exists (_ == a),
      o => this.obj exists (_ == o)
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

  type PossibleJson =
    Option[Json]

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

  import scalaz._, PLens._, CostateT._

  implicit def JsonJsonLike: JsonLike[Json] =
    new JsonLike[Json] {
      def isNull: Json => Boolean =
        _.fold(true, _ => false, _ => false, _ => false, _ => false, _ => false)

      def jBoolL: Json @-? Boolean =
        PLens(_.fold(None, z => Some(costate(jjBool, z)), _ => None, _ => None, _ => None, _ => None))

      def jNumberL: Json @-? JsonNumber =
        PLens(_.fold(None, _ => None, z => Some(costate(jjNumber, z)), _ => None, _ => None, _ => None))

      def jStringL: Json @-? JsonString =
        PLens(_.fold(None, _ => None, _ => None, z => Some(costate(jjString, z)), _ => None, _ => None))

      def jArrayL: Json @-? JsonArray =
        PLens(_.fold(None, _ => None, _ => None, _ => None, z => Some(costate(jjArray, z)), _ => None))

      def jObjectL: Json @-? JsonObject =
        PLens(_.fold(None, _ => None, _ => None, _ => None, _ => None, z => Some(costate(jjObject, z))))

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

  implicit def PossibleJsonJsonLike: JsonLike[PossibleJson] =
    new JsonLike[PossibleJson] {
      import JsonIdentity._
      def isNull: PossibleJson => Boolean =
        _.isEmpty

      def jBoolL: PossibleJson @-? Boolean =
        PLens(_ flatMap (_.bool map (costate(jBool, _))))

      def jNumberL: PossibleJson @-? JsonNumber =
        PLens(_ flatMap (_.number map (costate(jNumber, _))))

      def jStringL: PossibleJson @-? JsonString =
        PLens(_ flatMap (_.string map (costate(jString, _))))

      def jArrayL: PossibleJson @-? JsonArray =
        PLens(_ flatMap (_.array map (costate(jArray, _))))

      def jObjectL: PossibleJson @-? JsonObject =
        PLens(_ flatMap (_.obj map (costate(jObject, _))))

      def jNull =
        Some(jjNull)

      def jBool: Boolean => PossibleJson =
        x => Some(jjBool(x))

      def jNumber =
        x => Some(jjNumber(x))

      def jString =
        x => Some(jjString(x))

      def jArray =
        x => Some(jjArray(x))

      def jObject =
        x => Some(jjObject(x))
    }
}
