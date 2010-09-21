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

  /**
   * If this JSON value is `null` the return the given value, otherwise, the other value.
   *
   * @param t Returned if this JSON value is `null`.
   * @param f Returned if this JSON value is not `null`.
   */
  def ifNull[X](t: => X, f: => X) =
    fold(t,
         _ => f,
         _ => f,
         _ => f,
         _ => f,
         _ => f)

  /**
   * If this JSON value is a boolean (`true` or `false`), run the function on it, otherwise, return the other value.
   *
   * @param t The function to run if this JSON value is a boolean.
   * @param f Returned if this JSON value is not a boolean.
   */
  def ifBool[X](t: Boolean => X, f: => X) =
    fold(f,
         t(_),
         _ => f,
         _ => f,
         _ => f,
         _ => f)

   /**
   * If this JSON value is a number, run the function on it, otherwise, return the other value.
   *
   * @param t The function to run if this JSON value is a number.
   * @param f Returned if this JSON value is not a number.
   */
  def ifNumber[X](t: JsonNumber => X, f: => X) =
    fold(f,
         _ => f,
         t(_),
         _ => f,
         _ => f,
         _ => f)

  /**
   * If this JSON value is a string, run the function on it, otherwise, return the other value.
   *
   * @param t The function to run if this JSON value is a string.
   * @param f Returned if this JSON value is not a string.
   */
  def ifString[X](t: String => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         t(_),
         _ => f,
         _ => f)

  /**
   * If this JSON value is an array, run the function on it, otherwise, return the other value.
   *
   * @param t The function to run if this JSON value is an array.
   * @param f Returned if this JSON value is not an array.
   */
  def ifArray[X](t: JsonArray => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         _ => f,
         t(_),
         _ => f)

  /**
   * If this JSON value is an object, run the function on it, otherwise, return the other value.
   *
   * @param t The function to run if this JSON value is an object.
   * @param f Returned if this JSON value is not an object.
   */
  def ifObject[X](t: JsonObject => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         _ => f,
         _ => f,
         t(_))

  /**
   * Return `true` if this JSON value is `null`, otherwise, `false`.
   */
  def isNull =
    ifNull(true, false)

  /**
   * Return `true` if this JSON value is a boolean, otherwise, `false`.
   */
  def isBool =
    ifBool(_ => true, false)

  /**
   * Return `true` if this JSON value is a number, otherwise, `false`.
   */
  def isNumber =
    ifNumber(_ => true, false)

  /**
   * Return `true` if this JSON value is a string, otherwise, `false`.
   */
  def isString =
    ifString(_ => true, false)

  /**
   * Return `true` if this JSON value is an array, otherwise, `false`.
   */
  def isArray =
    ifArray(_ => true, false)

  /**
   * Return `true` if this JSON value is an object, otherwise, `false`.
   */
  def isObject =
    ifObject(_ => true, false)

  /**
   * Return `true` if this JSON value is a boolean with a `true` value, otherwise, `false`.
   */
  def ifBoolTrue[X](t: => X, f: => X) =
    ifBool(x => if(x) t else f, f)

  /**
   * Return `true` if this JSON value is a boolean with a `false` value, otherwise, `false`.
   */
  def ifBoolFalse[X](t: => X, f: => X) =
    ifBool(x => if(x) f else t, f)

  /**
   * Returns the possible number of this JSON value.
   */
  def number =
    ifNumber(Some(_), None)

  /**
   * Returns the number of this JSON value, or the given default if this JSON value is not a number.
   *
   * @param The default number if this JSON value is not a number.
   */
  def numberOr(d: => JsonNumber) =
    number getOrElse d

  def numberOrZero =
    numberOr(0D)

  /**
   * Returns the possible string of this JSON value.
   */
  def string =
    ifString(Some(_), None)

  /**
   * Returns the string of this JSON value, or the given default if this JSON value is not a string.
   *
   * @param The default string if this JSON value is not a string.
   */
  def stringOr(s: => String) =
    string getOrElse s

  /**
   * Returns the string of this JSON value, or an empty string if this JSON value is not a string.
   */
  def stringOrEmpty =
    stringOr("")

  /**
   * Returns the possible array of this JSON value.
   */
  def array =
    ifArray(Some(_), None)

  /**
   * Returns the array of this JSON value, or the given default if this JSON value is not an array.
   *
   * @param The default array if this JSON value is not an array.
   */
  def arrayOr(a: => JsonArray) =
    array getOrElse a

  /**
   * Returns the array of this JSON value, or an empty array if this JSON value is not an array.
   */
  def arrayOrEmpty =
    arrayOr(Nil)

  /**
   * Returns the possible object of this JSON value.
   */
  def objectt =
    ifObject(Some(_), None)

  /**
   * Returns the object of this JSON value, or the given default if this JSON value is not a object.
   *
   * @param The default object if this JSON value is not an object.
   */
  def objectOr(o: => JsonObject) =
    objectt getOrElse o

  /**
   * Returns the object of this JSON value, or the empty object if this JSON value is not an object.
   */
  def objectOrEmpty =
    objectOr(Nil)
  
  /**
   * Returns the possible object map of this JSON value.
   */
  lazy val objectMap: Option[JsonObjectMap] =
    fold(None,
         _ => None,
         _ => None,
         _ => None,
         _ => None,
         x => Some(x.toMap))

  /**
   * Returns the object map of this JSON value, or the given default if this JSON value is not an object.
   *
   * @param The default object map if this JSON value is not an object.
   */
  def objectMapOr(m: => JsonObjectMap) =
    objectMap getOrElse m

  /**
   * Returns the object map of this JSON value, or the empty map if this JSON value is not an object.
   */
  def objectMapOrEmpty =
    objectMapOr(Map.empty)

  def objectValue(k: String): Option[Json] =
    objectMap flatMap (_ get k)

  def objectValueOr(k: String, v: => Json) =
    objectValue(k) getOrElse v

  def not =
    ifBool(x => jsonBool(!x), this)

  def withNumber(f: JsonNumber => JsonNumber) =
    ifNumber(jsonNumber compose f, this)

  def withArray(f: JsonArray => JsonArray) =
    ifArray(jsonArray compose f, this)

  def withObject(f: JsonObject => JsonObject) =
    ifObject(jsonObject compose f, this)

  def objectKeys =
    ifObject(_ map (_._1), Nil)

  def objectValue =
    ifObject(_ map (_._2), Nil)

  /**
   * If this is a JSON object, then prepend the given value, otherwise, return a JSON object with only the given value.
   */
  def ->:(obj: (String, Json)) =
    jsonObject(ifObject(obj :: _, List(obj)))

  /**
   * If this is a JSON array, then prepend the given value, otherwise, return a JSON array with only the given value.
   */
  def -->>:(ar: Json) =
    jsonArray(ifArray(ar :: _, List(ar)))

  /**
   * If this is a JSON object, then prepend the given value, otherwise, return this.
   */
  def =>:(obj: (String, Json)) =
    withObject(obj :: _)

  /**
   * If this is a JSON array, then prepend the given value, otherwise, return this.
   */
  def ==>>:(ar: Json) =
    withArray(ar :: _)
}

object Json {
  type JsonNumber = Double
  type JsonArray = List[Json]
  type JsonObject = List[(String, Json)]
  type JsonObjectMap = Map[String, Json]

  val jsonNull: Json = new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jnull 
  }

  val jsonBool: Boolean => Json = (b: Boolean) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jbool(b)
  }

  val jsonNumber: JsonNumber => Json = (n: JsonNumber) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jnumber(n)
  }

  val jsonString: String => Json = (s: String) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jstring(s)
  }

  val jsonArray: JsonArray => Json = (a: JsonArray) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jarray(a)
  }

  val jsonObject: JsonObject => Json = (x: JsonObject) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: JsonNumber => X,
      jstring: String => X,
      jarray: JsonArray => X,
      jobject: JsonObject => X
    ) = jobject(x) 
  }

  val jsonObjectMap = (x: JsonObjectMap) => jsonObject(x.toList)
}
