package com.ephox.argonaut

trait JsonValue[A] {
  import JsonValue._

  def fold[X](
    error: String => X,
    value: A => X
  ): X

  def map[B](f: A => B): JsonValue[B] = fold(
    e => jsonError(e),
    a => jsonValue(f(a))
  )

  def flatMap[B](f: A => JsonValue[B]): JsonValue[B] = fold(
    e => jsonError(e),
    a => f(a)
  )
}

object JsonValue {
  def jsonValue[A](a: A): JsonValue[A] = new JsonValue[A] {
    def fold[X](
      error: String => X,
      value: A => X
    ): X = value(a)
  }
  
  def jsonError[A](message: String): JsonValue[A] = new JsonValue[A] {
    def fold[X](
      error: String => X,
      value: A => X
    ): X = error(message)
  }
}
