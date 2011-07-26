package com.ephox.argonaut

trait ToJson[A] {
  def apply(a: A): Json
}

object ToJson extends ToJsons

trait ToJsons {
  def toJson[A](f: A => Json): ToJson[A] = new ToJson[A] {
    def apply(a: A) = f(a)
  }

  import Json._

  implicit def JIntegerToJson: ToJson[java.lang.Integer] =
    toJson(v => jNumber(v.intValue.toDouble))

  implicit def JLongToJson: ToJson[java.lang.Long] =
    toJson(v => jNumber(v.longValue.toDouble))

  implicit def JBooleanToJson: ToJson[java.lang.Boolean] =
    toJson(v => jBool(v.booleanValue))

  implicit def BooleanToJson: ToJson[Boolean] =
    toJson(jBool)

  implicit def IntToJson: ToJson[Int] =
    toJson(v => jNumber(v.toDouble))

  implicit def LongToJson: ToJson[Long] =
    toJson(v => jNumber(v.toDouble))

  implicit def DoubleToJson: ToJson[Double] =
    toJson(jNumber)

  implicit def StringToJson: ToJson[String] =
    toJson(jString)

  implicit def ListToJsonArray: ToJson[List[Json]] =
    toJson(jArray)

  implicit def ListToJsonObject: ToJson[List[(String, Json)]] =
    toJson(jObject)

  implicit def MapToJsonObject: ToJson[Map[String, Json]] =
    toJson(jObjectMap)

  implicit def ToJsonListAsJson[A](implicit to: ToJson[A]): ToJson[List[A]] =
    toJson(as => as.foldRight(jEmptyArray)((a, acc) => to(a) -->>: acc))
}