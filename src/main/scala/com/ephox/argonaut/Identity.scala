package com.ephox.argonaut

sealed trait Identity[A] {
  val value: A

  def pjson(implicit c: ContraPossibleJson[A]) = c.copjson(value)
}

object Identity {
  implicit def ToIdentity[A](a: A): Identity[A] = new Identity[A] {
    val value = a
  }
}
