package com.ephox.argonaut

trait AsJsons {
  implicit def ToJsonAsJson[A](a: A)(implicit to: ToJson[A]) = new {
    def json = to(a)
  }
}

object AsJson extends AsJsons