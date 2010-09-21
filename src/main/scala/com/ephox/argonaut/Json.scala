package com.ephox.argonaut

sealed trait Json {
  import Json._

  def fold[X](
    jsonNull: => X,
    jsonBool: Boolean => X,
    jsonNumber: JsonNumber => X,
    jsonString: String => X,
    jsonArray: JsonArray => X,
    jsonObject: JsonObject => X
  ): X

  def ifNull[X](t: => X, f: => X) =
    fold(t,
         _ => f,
         _ => f,
         _ => f,
         _ => f,
         _ => f)

  def ifBool[X](t: Boolean => X, f: => X) =
    fold(f,
         t(_),
         _ => f,
         _ => f,
         _ => f,
         _ => f)

  def ifNumber[X](t: JsonNumber => X, f: => X) =
    fold(f,
         _ => f,
         t(_),
         _ => f,
         _ => f,
         _ => f)

  def ifString[X](t: String => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         t(_),
         _ => f,
         _ => f)

  def ifArray[X](t: JsonArray => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         _ => f,
         t(_),
         _ => f)

  def ifObject[X](t: JsonObject => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         _ => f,
         _ => f,
         t(_))

  def isNull =
    ifNull(true, false)

  def isBool =
    ifBool(_ => true, false)

  def isNumber =
    ifNumber(_ => true, false)

  def isString =
    ifString(_ => true, false)

  def isArray =
    ifArray(_ => true, false)

  def isObject =
    ifObject(_ => true, false)

  def ifBoolTrue[X](t: => X, f: => X) =
    ifBool(x => if(x) t else f, f)

  def ifBoolFalse[X](t: => X, f: => X) =
    ifBool(x => if(x) f else t, f)

  def number =
    ifNumber(Some(_), None)

  def numberOr(d: => Number) =
    number getOrElse d

  def numberOrZero =
    numberOr(0D)

  def string =
    ifString(Some(_), None)

  def stringOr(s: => String) =
    string getOrElse s

  def stringOrEmpty =
    stringOr("")

  def array =
    ifArray(Some(_), None)

  def arrayOr(a: => JsonArray) =
    array getOrElse a

  def arrayOrEmpty =
    arrayOr(Nil)

  def objectt =
    ifObject(Some(_), None)

  def objectOr(o: => JsonObject) =
    objectt getOrElse o

  def objectOrEmpty =
    objectOr(Nil)
  
  lazy val objectMap: Option[JsonObjectMap] =
    fold(None,
         _ => None,
         _ => None,
         _ => None,
         _ => None,
         x => Some(x.toMap))

  def objectMapOr(m: => JsonObjectMap) =
    objectMap getOrElse m

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
