package com.ephox
package argonaut

import scalaz._, Scalaz._, LensT._

import Json._

/**
 * Encode an arbitrary value as a JSON value.
 *
 * @author Tony Morris
 */
trait EncodeJson[-A] {
  def name: String

  /**
   * Encode the given value.
   */
  def apply(a: A): Json

  /**
   * Contravariant functor.
   */
  def contramap[B](f: B => A): EncodeJson[B] =
    EncodeJson(b => apply(f(b)), name)
}

object EncodeJson extends EncodeJsons {
  def apply[A](f: A => Json, n: String): EncodeJson[A] =
    new EncodeJson[A] {
      def name = n
      def apply(a: A) = f(a)
    }
}

trait EncodeJsons {
  implicit def IdEncodeJson: EncodeJson[Json] =
    EncodeJson(q => q, "Json")

  implicit def ListEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[List[A]] =
    EncodeJson(a => jArray(a map (e(_))), "[A]List[A]")

  implicit def StreamEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Stream[A]] =
    EncodeJson(a => jArray(a.toList map (e(_))), "[A]Stream[A]")

  implicit def StringEncodeJson: EncodeJson[String] =
    EncodeJson(jString, "String")

  implicit def DoubleEncodeJson: EncodeJson[Double] =
    EncodeJson(jDouble, "Double")

  implicit def FloatEncodeJson: EncodeJson[Float] =
    EncodeJson(a => jDouble(a), "Float")

  implicit def IntEncodeJson: EncodeJson[Int] =
    EncodeJson(a => jString(a.toString), "Int")

  implicit def LongEncodeJson: EncodeJson[Long] =
    EncodeJson(a => jString(a.toString), "Long")

  implicit def BooleanEncodeJson: EncodeJson[Boolean] =
    EncodeJson(jBool, "Boolean")

  implicit def CharEncodeJson: EncodeJson[Char] =
    EncodeJson(a => jString(a.toString), "Char")

  implicit def JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    EncodeJson(a => jDouble(a.doubleValue), "java.lang.Double")

  implicit def JFloatEncodeJson: EncodeJson[java.lang.Float] =
    EncodeJson(a => jDouble(a.floatValue.toDouble), "java.lang.Float")

  implicit def JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    EncodeJson(a => jString(a.toString), "java.lang.Integer")

  implicit def JLongEncodeJson: EncodeJson[java.lang.Long] =
    EncodeJson(a => jString(a.toString), "java.lang.Long")

  implicit def JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    EncodeJson(a => jBool(a.booleanValue), "java.lang.Boolean")

  implicit def JCharacterEncodeJson: EncodeJson[java.lang.Character] =
    EncodeJson(a => jString(a.toString), "java.lang.Character")

  implicit def OptionEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Option[A]] =
    EncodeJson(_ match {
      case None => jNull
      case Some(a) => e(a)
    }, "[A]Option[A]")

  implicit def EitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Either[A, B]] =
    EncodeJson(_ match {
      case Left(a) => jSingleObject("Left", ea(a))
      case Right(b) => jSingleObject("Right", eb(b))
    }, "[A, B]Either[A, B]")

  implicit def ValidationEncodeJson[E, A](implicit ea: EncodeJson[E], eb: EncodeJson[A]): EncodeJson[Validation[E, A]] =
    EncodeJson(_ fold (
      failure = e => jSingleObject("Failure", ea(e))
    , success = a => jSingleObject("Success", eb(a))
    ), "[E, A]Validation[E, A]")

  implicit def MapEncodeJson[V](implicit e: EncodeJson[V]): EncodeJson[Map[String, V]] =
    EncodeJson(x => jObjectAssocList(
      x.toList map {
        case (k, v) => (k, e(v))
      }
    ), "[V]Map[String, V]")

  implicit def SetEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Set[A]] =
    EncodeJson(ListEncodeJson[A] contramap ((_: Set[A]).toList) apply _, "[A]Set[A]")

  implicit def Tuple2EncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[(A, B)] =
    EncodeJson({
      case (a, b) => jArray(List(ea(a), eb(b)))
    }, "[A, B](A, B)")

  implicit def Tuple3EncodeJson[A, B, C](implicit ea: EncodeJson[A], eb: EncodeJson[B], ec: EncodeJson[C]): EncodeJson[(A, B, C)] =
    EncodeJson({
      case (a, b, c) => jArray(List(ea(a), eb(b), ec(c)))
    }, "[A, B, C](A, B, C)")


  implicit def EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }
}