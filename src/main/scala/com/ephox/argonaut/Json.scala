package com.ephox.argonaut

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
   * If this is a JSON boolean value, invert the `true` and `false` values, otherwise, leave unchanged.
   */
  def not =
    if(p.isTrue) jFalse
    else if(p.isFalse) jTrue
    else this

  /**
   * If this is a JSON number value, run the given function on the value, otherwise, leave unchanged.
   */
  def withNumber(k: JsonNumber => JsonNumber) = p.number match {
    case Some(d) => jNumber(k(d))
    case None => this
  }

  /**
   * If this is a JSON string value, run the given function on the value, otherwise, leave unchanged.
   */
  def withString(k: JsonString => JsonString) = p.string match {
    case Some(s) => jString(k(s))
    case None => this
  }

  /**
   * If this is a JSON array value, run the given function on the value, otherwise, leave unchanged.
   */
  def withArray(k: JsonArray => JsonArray) = p.array match {
    case Some(a) => jArray(k(a))
    case None => this
  }

  /**
   * If this is a JSON object value, run the given function on the value, otherwise, leave unchanged.
   */
  def withObject(k: JsonObject => JsonObject) = p.objectt match {
    case Some(o) => jObject(k(o))
    case None => this
  }

  /**
   * If this is a JSON object, then prepend the given value, otherwise, return a JSON object with only the given value.
   */
  def ->:(j: => JsonAssoc) = withObject(j :: _)

  /**
   * If this is a JSON array, then prepend the given value, otherwise, return a JSON array with only the given value.
   */
  def -->>:(j: => Json) = withArray(j :: _)

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
      o => p.objectt exists (_ == o)
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
    "Json { " +
        fold(
          "null",
          "bool   [" + _ + "]",
          "number [" + _ + "]",
          "string [" + _ + "]",
          "array  [" + _ + "]",
          "object [" + _ + "]"
        ) + " }"
}

/**
 * Constructors and other utilities for JSON values.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
object Json {
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

  /**
   * Implicitly convert to a possible JSON.
   */
  implicit def JsonPossible(j: Json): PossibleJson = PossibleJson.pJson(j)
}
