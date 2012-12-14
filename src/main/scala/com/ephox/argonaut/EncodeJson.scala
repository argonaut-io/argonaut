package com.ephox
package argonaut

import scalaz._, Scalaz._, LensT._

import Json._

/**
 * Encode an arbitrary value as a JSON value.
 *
 * @author Tony Morris
 */
sealed trait EncodeJson[-A] {
  val name: String

  /**
   * Encode the given value.
   */
  def apply(a: A): Json

  /**
   * Contravariant functor.
   */
  def contramap[B](f: B => A): EncodeJson[B] =
    EncodeJson(b => apply(f(b)), name)

  /**
   * Set the name of this encoder.
   */
  def setName(n: String): EncodeJson[A] =
    EncodeJson(apply(_), n)
}

object EncodeJson extends EncodeJsons {
  def apply[A](f: A => Json, n: String): EncodeJson[A] =
    new EncodeJson[A] {
      val name = n
      def apply(a: A) = f(a)
    }
}

trait EncodeJsons {
  def encodeJsonNameL[A]: EncodeJson[A] @> String =
    Lens(e => Store(EncodeJson(e(_), _), e.name))

  implicit val IdEncodeJson: EncodeJson[Json] =
    EncodeJson(q => q, "Json")

  implicit val UnitEncodeJson: EncodeJson[Unit] =
    EncodeJson(_ => jEmptyObject, "Unit")

  implicit def ListEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[List[A]] =
    EncodeJson(a => jArray(a map (e(_))), "[A]List[A]")

  implicit def StreamEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Stream[A]] =
    EncodeJson(a => jArray(a.toList map (e(_))), "[A]Stream[A]")

  implicit val StringEncodeJson: EncodeJson[String] =
    EncodeJson(jString, "String")

  implicit val DoubleEncodeJson: EncodeJson[Double] =
    EncodeJson(jDouble, "Double")

  implicit val FloatEncodeJson: EncodeJson[Float] =
    EncodeJson(a => jDouble(a), "Float")

  implicit val IntEncodeJson: EncodeJson[Int] =
    EncodeJson(a => jString(a.toString), "Int")

  implicit val LongEncodeJson: EncodeJson[Long] =
    EncodeJson(a => jString(a.toString), "Long")

  implicit val BooleanEncodeJson: EncodeJson[Boolean] =
    EncodeJson(jBool, "Boolean")

  implicit val CharEncodeJson: EncodeJson[Char] =
    EncodeJson(a => jString(a.toString), "Char")

  implicit val JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    EncodeJson(a => jDouble(a.doubleValue), "java.lang.Double")

  implicit val JFloatEncodeJson: EncodeJson[java.lang.Float] =
    EncodeJson(a => jDouble(a.floatValue.toDouble), "java.lang.Float")

  implicit val JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    EncodeJson(a => jString(a.toString), "java.lang.Integer")

  implicit val JLongEncodeJson: EncodeJson[java.lang.Long] =
    EncodeJson(a => jString(a.toString), "java.lang.Long")

  implicit val JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    EncodeJson(a => jBool(a.booleanValue), "java.lang.Boolean")

  implicit val JCharacterEncodeJson: EncodeJson[java.lang.Character] =
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
      e => jSingleObject("Failure", ea(e))
    , a => jSingleObject("Success", eb(a))
    ), "[E, A]Validation[E, A]")

  implicit def MapEncodeJson[V](implicit e: EncodeJson[V]): EncodeJson[Map[String, V]] =
    EncodeJson(x => jObjectAssocList(
      x.toList map {
        case (k, v) => (k, e(v))
      }
    ), "[V]Map[String, V]")

  implicit def SetEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Set[A]] =
    EncodeJson(ListEncodeJson[A] contramap ((_: Set[A]).toList) apply _, "[A]Set[A]")

  implicit def Tuple1EncodeJson[A](implicit ea: EncodeJson[A]): EncodeJson[Tuple1[A]] =
    EncodeJson(a => jArray(List(ea(a._1))), "[A](A)")

  implicit def Tuple2EncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[(A, B)] =
    EncodeJson({
      case (a, b) => jArray(List(ea(a), eb(b)))
    }, "[A, B](A, B)")

  implicit def Tuple3EncodeJson[A, B, C](implicit ea: EncodeJson[A], eb: EncodeJson[B], ec: EncodeJson[C]): EncodeJson[(A, B, C)] =
    EncodeJson({
      case (a, b, c) => jArray(List(ea(a), eb(b), ec(c)))
    }, "[A, B, C](A, B, C)")

  implicit def Tuple4EncodeJson[A, B, C, D](implicit ea: EncodeJson[A], eb: EncodeJson[B], ec: EncodeJson[C], ed: EncodeJson[D]): EncodeJson[(A, B, C, D)] =
    EncodeJson({
      case (a, b, c, d) => jArray(List(ea(a), eb(b), ec(c), ed(d)))
    }, "[A, B, C, D](A, B, C, D)")


  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }

  def jencode1[X, A: EncodeJson](f: X => A): EncodeJson[X] =
    implicitly[EncodeJson[A]].contramap(f)

  def jencode2[X, A: EncodeJson, B: EncodeJson](f: X => (A, B)): EncodeJson[X] =
    implicitly[EncodeJson[(A, B)]].contramap(f)

  def jencode3[X, A: EncodeJson, B: EncodeJson, C: EncodeJson](f: X => (A, B, C)): EncodeJson[X] =
    implicitly[EncodeJson[(A, B, C)]].contramap(f)

  def jencode4[X, A: EncodeJson, B: EncodeJson, C: EncodeJson, D: EncodeJson](f: X => (A, B, C, D)): EncodeJson[X] =
    implicitly[EncodeJson[(A, B, C, D)]].contramap(f)

  def jencode1L[X, A: EncodeJson](f: X => A)(an: JsonString, bn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val a = f(x)
      List((an, implicitly[EncodeJson[A]] apply a))
    }), "[A]Map[String, A]")

  def jencode2L[X, A: EncodeJson, B: EncodeJson](f: X => (A, B))(an: JsonString, bn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b))
    }), "[A, B]Map[String, A|B]")

  def jencode3L[X, A: EncodeJson, B: EncodeJson, C: EncodeJson](f: X => (A, B, C))(an: JsonString, bn: JsonString, cn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b, c) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b), (cn, implicitly[EncodeJson[C]] apply c))
    }), "[A, B, C]Map[String, A|B|C]")

  def jencode4L[X, A: EncodeJson, B: EncodeJson, C: EncodeJson, D: EncodeJson](f: X => (A, B, C, D))(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b, c, d) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b), (cn, implicitly[EncodeJson[C]] apply c), (dn, implicitly[EncodeJson[D]] apply d))
    }), "[A, B, C, D]Map[String, A|B|C|D]")

  def jencode5L[X, A: EncodeJson, B: EncodeJson, C: EncodeJson, D: EncodeJson, E: EncodeJson](f: X => (A, B, C, D, E))(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString, en: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b, c, d, e) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b), (cn, implicitly[EncodeJson[C]] apply c), (dn, implicitly[EncodeJson[D]] apply d), (en, implicitly[EncodeJson[E]] apply e))
    }), "[A, B, C, D, E]Map[String, A|B|C|D|E]")

}
