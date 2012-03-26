package com.ephox
package argonaut

trait AsJsons {
  implicit def ToJsonAsJson[A](a: A)(implicit to: ToJson[A]) = new {
    def asJson = to(a)
  }
}

object AsJson extends AsJsons
