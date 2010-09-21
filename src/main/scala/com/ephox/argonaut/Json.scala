package com.ephox.argonaut

sealed trait Json {
  def fold[X](
    jsonNull: => X,
    jsonBool: Boolean => X,
    jsonNumber: Double => X,
    jsonString: String => X,
    jsonArray: List[Json] => X,
    jsonObject: List[(String, Json)] => X
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

  def ifNumber[X](t: Double => X, f: => X) =
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

  def ifArray[X](t: List[Json] => X, f: => X) =
    fold(f,
         _ => f,
         _ => f,
         _ => f,
         t(_),
         _ => f)

  def ifObject[X](t: List[(String, Json)] => X, f: => X) =
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

  def numberOr(d: => Double) =
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

  def arrayOr(a: => List[Json]) =
    array getOrElse a

  def arrayOrEmpty =
    arrayOr(Nil)

  def objectt =
    ifObject(Some(_), None)

  def objectOr(o: => List[(String, Json)]) =
    objectt getOrElse o

  def objectOrEmpty =
    objectOr(Nil)
  
  lazy val objectMap: Option[Map[String, Json]] =
    fold(None,
         _ => None,
         _ => None,
         _ => None,
         _ => None,
         x => Some(x.toMap))

  def objectMapOr(m: => Map[String, Json]) =
    objectMap getOrElse m

  def objectMapOrEmpty =
    objectMapOr(Map.empty)

  def objectValue(k: String): Option[Json] =
    objectMap flatMap (_ get k)

  def objectValueOr(k: String, v: => Json) =
    objectValue(k) getOrElse v
}

object Json {
  val jsonNull: Json = new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: Double => X,
      jstring: String => X,
      jarray: List[Json] => X,
      jobject: List[(String, Json)] => X
    ) = jnull 
  }

  val jsonBool: Boolean => Json = (b: Boolean) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: Double => X,
      jstring: String => X,
      jarray: List[Json] => X,
      jobject: List[(String, Json)] => X
    ) = jbool(b)
  }

  val jsonNumber: Double => Json = (n: Double) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: Double => X,
      jstring: String => X,
      jarray: List[Json] => X,
      jobject: List[(String, Json)] => X
    ) = jnumber(n)
  }

  val jsonString: String => Json = (s: String) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: Double => X,
      jstring: String => X,
      jarray: List[Json] => X,
      jobject: List[(String, Json)] => X
    ) = jstring(s)
  }

  val jsonArray: List[Json] => Json = (a: List[Json]) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: Double => X,
      jstring: String => X,
      jarray: List[Json] => X,
      jobject: List[(String, Json)] => X
    ) = jarray(a)
  }

  val jsonObject: List[(String, Json)] => Json = (x: List[(String, Json)]) => new Json {
    def fold[X](
      jnull: => X,
      jbool: Boolean => X,
      jnumber: Double => X,
      jstring: String => X,
      jarray: List[Json] => X,
      jobject: List[(String, Json)] => X
    ) = jobject(x) 
  }

  val jsonObjectMap = (x: Map[String, Json]) => jsonObject(x.toList)
}
