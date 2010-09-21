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

  /*
  def foldMMM[X](
    jsonNull: => X,
    jsonBool: Boolean => X,
    jsonNumber: Double => X,
    jsonString: String => X,
    jsonArray: List[Json] => X,
    jsonObject: Map[String, Json] => X
  ) = fold(jsNull, jsonBool, jsonNumber, jsonString, jsonArray, x => jsonObject(x.toMap))

  val possibleMap: Option[Map[String, Json]] =
    foldMMM(None, _ => None, _ => None, _ => None, _ => None, _ => None, Some(_))
  */

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

  val jsonBoolString = (s: List[Char]) => jsonBool("true".toList == s)
}
