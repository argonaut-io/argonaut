package com.ephox.argonaut

/**
 * A wrapper on any value providing methods.
 *
 * @author Tony Morris
 */
sealed trait Identity[A] {
  /**
   * The wrapped value.
   */
  val value: A

  /**
   * Convert this value to a possible JSON value using the given contravariant implicit.
   */
  def pjson(implicit c: ContraPossibleJson[A]) = c.copjson(value)
}

/**
 * Constructors and other utilities for wrapped values.
 *
 * @author Tony Morris
 */
object Identity {
  /**
   * Implicitly wraps a value.
   */
  implicit def ToIdentity[A](a: A): Identity[A] = new Identity[A] {
    val value = a
  }
}
