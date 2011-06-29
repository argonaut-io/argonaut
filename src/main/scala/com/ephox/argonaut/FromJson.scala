package com.ephox.argonaut

trait FromJson[A] {
  import FromJson._

  def apply(json: Json): JsonValue[A]

  def map[B](f: A => B): FromJson[B] =
    fromJson(json => apply(json) map f)

  def flatMap[B](f: A => FromJson[B]): FromJson[B] =
    fromJson(json => apply(json) flatMap (a => f(a)(json)))
}

object FromJson extends FromJsons

trait FromJsons {
  def fromJson[A](f: Json => JsonValue[A]): FromJson[A] = new FromJson[A] {
    def apply(json: Json) = f(json)
  }
}