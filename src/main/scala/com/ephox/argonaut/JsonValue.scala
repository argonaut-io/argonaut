package com.ephox
package argonaut

import scalaz._, Scalaz._

trait JsonValue[A] {
  import JsonValue._

  def fold[X](
    error: String => X,
    value: A => X
  ): X

  def toOption =
    fold(_ => None, Some(_))

  def getOr(v: => A) =
    toOption.getOrElse(v)

  def map[B](f: A => B): JsonValue[B] = fold(
    e => jsonError(e),
    a => jsonValue(f(a))
  )

  def flatMap[B](f: A => JsonValue[B]): JsonValue[B] = fold(
    e => jsonError(e),
    a => f(a)
  )

  def mapError(f: String => String): JsonValue[A] =
    flatMapError(s => jsonError(f(s)))

  def flatMapError(f: String => JsonValue[A]): JsonValue[A] = fold(
    e => f(e),
    a => jsonValue(a)
  )
}

object JsonValue {
  def jsonValue[A]: A => JsonValue[A] = a => new JsonValue[A] {
    def fold[X](
      error: String => X,
      value: A => X
    ): X = value(a)
  }
  
  def jsonError[A]: String => JsonValue[A] = message => new JsonValue[A] {
    def fold[X](
      error: String => X,
      value: A => X
    ): X = error(message)
  }

  implicit def JsonValueMonad: Monad[JsonValue] = new Monad[JsonValue] {
    def point[A](a: => A) = jsonValue(a)
    def bind[A, B](a: JsonValue[A])(f: A => JsonValue[B]) = a flatMap f
    override def map[A, B](a: JsonValue[A])(f: A => B) = a map f
  }
}
