package com.ephox.argonaut

import Json._

trait AsJsons {
  def asJson[A](f: => Json) = new {
    def json = f
  }

  implicit def JIntegerAsJson(v: java.lang.Integer) =
    IntAsJson(v.intValue)

  implicit def JLongAsJson(v: java.lang.Long) =
    LongAsJson(v.longValue())

  implicit def JBooleanAsJson(v: java.lang.Boolean) =
    BooleanAsJson(v.booleanValue)

  implicit def BooleanAsJson(v: Boolean) =
    asJson(jBool(v))

  implicit def IntAsJson(v: Int) =
    asJson(jNumber(v.toDouble))

  implicit def LongAsJson(v: Long) =
    asJson(jNumber(v.toDouble))

  implicit def DoubleAsJson(v: Double) =
    asJson(jNumber(v))

  implicit def StringAsJson(v: String) =
    asJson(jString(v))

  implicit def ListAsJsonArray(v: List[Json]) =
    asJson(jArray(v))

  implicit def ListAsJsonObject(v: List[(String, Json)]) =
    asJson(jObject(v))

  implicit def MapAsJsonObject(v: Map[String, Json]) =
    asJson(jObjectMap(v))

  implicit def ToJsonAsJson[A](a: A)(implicit to: ToJson[A]) =
    asJson(to(a))

  implicit def ToJsonListAsJson[A](as: List[A])(implicit to: ToJson[A]) =
    asJson(as.foldRight(jEmptyArray)((a, acc) => to(a) -->>: acc))
}

object AsJson extends AsJsons