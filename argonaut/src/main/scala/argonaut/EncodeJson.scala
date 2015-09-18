package argonaut

import Json._

/**
 * Encode an arbitrary value as a JSON value.
 *
 * @author Tony Morris
 */
trait EncodeJson[A] {
  /**
   * Encode the given value. Alias for `encode`.
   */
  def apply(a: A): Json =
    encode(a)

  /**
   * Encode the given value.
   */
  def encode(a: A): Json

  /**
   * Contravariant functor.
   */
  def contramap[B](f: B => A): EncodeJson[B] =
    EncodeJson(b => apply(f(b)))

  /**
   * Split on this encoder and the given encoder.
   */
  def <&>[B](x: => EncodeJson[B]): EncodeJson[Either[A, B]] =
    EncodeJson {
      case Left(a) => apply(a)
      case Right(b) => x(b)
    }

}

object EncodeJson extends EncodeJsons {
  def apply[A](f: A => Json): EncodeJson[A] =
    new EncodeJson[A] {
      def encode(a: A) = f(a)
    }

  def derive[A]: EncodeJson[A] = macro internal.Macros.materializeEncodeImpl[A]

  def of[A: EncodeJson] = implicitly[EncodeJson[A]]
}

trait EncodeJsons extends GeneratedEncodeJsons {
  /* TODO: Come back to this.
  def contrazip[A, B](e: EncodeJson[A \/ B]): (EncodeJson[A], EncodeJson[B]) =
    (EncodeJson(a => e(a.left)), EncodeJson(b => e(b.right)))
  */

  implicit val JsonEncodeJson: EncodeJson[Json] =
    EncodeJson(q => q)

  implicit val HCursorEncodeJson: EncodeJson[HCursor] =
    EncodeJson(q => q.focus)

  implicit val UnitEncodeJson: EncodeJson[Unit] =
    EncodeJson(_ => jEmptyObject)

  implicit def TraversableOnceEncodeJson[A0, C[_]](implicit
    e: EncodeJson[A0],
    is: collection.generic.IsTraversableOnce[C[A0]] { type A = A0 }
  ): EncodeJson[C[A0]] =
    EncodeJson(a => jArray(is.conversion(a).toList.map(e(_))))

  implicit val StringEncodeJson: EncodeJson[String] =
    EncodeJson(jString)

  implicit val DoubleEncodeJson: EncodeJson[Double] =
    EncodeJson(a => JsonDouble(a).asJsonOrNull)

  implicit val FloatEncodeJson: EncodeJson[Float] =
    EncodeJson(a => JsonDouble(a.toDouble).asJsonOrNull)

  implicit val IntEncodeJson: EncodeJson[Int] =
    EncodeJson(a => JsonLong(a.toLong).asJsonOrNull)

  implicit val LongEncodeJson: EncodeJson[Long] =
    EncodeJson(a => JsonLong(a).asJsonOrNull)

  implicit val ShortEncodeJson: EncodeJson[Short] =
    EncodeJson(a => JsonLong(a.toLong).asJsonOrNull)

  implicit val ByteEncodeJson: EncodeJson[Byte] =
    EncodeJson(a => JsonLong(a.toLong).asJsonOrNull)

  implicit val BigDecimalEncodeJson: EncodeJson[BigDecimal] =
    EncodeJson(a => JsonBigDecimal(a).asJsonOrNull)

  implicit val BigIntEncodeJson: EncodeJson[BigInt] =
    EncodeJson(a => JsonBigDecimal(BigDecimal(a, java.math.MathContext.UNLIMITED)).asJsonOrNull)

  implicit val BooleanEncodeJson: EncodeJson[Boolean] =
    EncodeJson(jBool)

  implicit val CharEncodeJson: EncodeJson[Char] =
    EncodeJson(a => jString(a.toString))

  implicit val JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    EncodeJson(a => JsonDouble(a.doubleValue).asJsonOrNull)

  implicit val JFloatEncodeJson: EncodeJson[java.lang.Float] =
    EncodeJson(a => JsonDouble(a.floatValue.toDouble).asJsonOrNull)

  implicit val JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    EncodeJson(a => JsonLong(a.intValue.toLong).asJsonOrNull)

  implicit val JLongEncodeJson: EncodeJson[java.lang.Long] =
    EncodeJson(a => JsonLong(a.longValue).asJsonOrNull)

  implicit val JShortEncodeJson: EncodeJson[java.lang.Short] =
    EncodeJson(a => JsonLong(a.shortValue.toLong).asJsonOrNull)

  implicit val JByteEncodeJson: EncodeJson[java.lang.Byte] =
    EncodeJson(a => JsonLong(a.shortValue.toByte).asJsonOrNull)

  implicit val JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    EncodeJson(a => jBool(a.booleanValue))

  implicit val JCharacterEncodeJson: EncodeJson[java.lang.Character] =
    EncodeJson(a => jString(a.toString))

  implicit def OptionEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Option[A]] =
    EncodeJson(_ match {
      case None    => jNull
      case Some(a) => e(a)
    })

  implicit def EitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Either[A, B]] =
    EncodeJson(_ match {
      case Left(a)  => jSingleObject("Left", ea(a))
      case Right(b) => jSingleObject("Right", eb(b))
    })

  implicit def MapLikeEncodeJson[M[K, +V] <: Map[K, V], K, V](implicit K: EncodeJsonKey[K], e: EncodeJson[V]): EncodeJson[M[K, V]] =
    EncodeJson(x => jObjectAssocList(
      x.toList map {
        case (k, v) => (K.toJsonKey(k), e(v))
      }
    ))
}
