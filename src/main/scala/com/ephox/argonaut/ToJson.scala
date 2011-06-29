package com.ephox.argonaut

trait ToJson[A] {
  def apply(a: A): Json
}

object ToJson extends ToJsons

trait ToJsons {
  def toJson[A](f: A => Json): ToJson[A] = new ToJson[A] {
    def apply(a: A) = f(a)
  }
}