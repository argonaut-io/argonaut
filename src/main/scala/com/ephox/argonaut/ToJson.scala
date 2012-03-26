package com.ephox
package argonaut

trait ToJson[A] {
  def apply(a: A): Json
}

object ToJson extends ToJsons

trait ToJsons {
  def toJson[A](f: A => Json): ToJson[A] = new ToJson[A] {
    def apply(a: A) = f(a)
  }

  import JsonLike._
  import JsonIdentity._

  implicit def JIntegerToJson: ToJson[java.lang.Integer] =
    toJson(v => jNumber[Json](v.intValue.toDouble))

  implicit def JLongToJson: ToJson[java.lang.Long] =
    toJson(v => jNumber[Json](v.longValue.toDouble))

  implicit def JBooleanToJson: ToJson[java.lang.Boolean] =
    toJson(v => jBool[Json](v.booleanValue))

  implicit def BooleanToJson: ToJson[Boolean] =
    toJson(jBool[Json])

  implicit def IntToJson: ToJson[Int] =
    toJson(v => jNumber[Json](v.toDouble))

  implicit def LongToJson: ToJson[Long] =
    toJson(v => jNumber[Json](v.toDouble))

  implicit def DoubleToJson: ToJson[Double] =
    toJson(jNumber[Json])

  implicit def StringToJson: ToJson[String] =
    toJson(jString[Json])

  implicit def ListToJsonArray: ToJson[List[Json]] =
    toJson(jArray[Json])

  implicit def ListToJsonObject: ToJson[List[(String, Json)]] =
    toJson(jObject[Json])

  implicit def MapToJsonObject: ToJson[Map[String, Json]] =
    toJson(jObjectMap[Json])

  implicit def ToJsonListAsJson[A](implicit to: ToJson[A]): ToJson[List[A]] =
    toJson(as => as.foldRight(jEmptyArray[Json])((a, acc) => to(a) -->>: acc))
}