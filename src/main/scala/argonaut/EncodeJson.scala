package argonaut

import scalaz._, syntax.either._
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
  def <&>[B](x: => EncodeJson[B]): EncodeJson[A \/ B] =
    EncodeJson {
      case -\/(a) => apply(a)
      case \/-(b) => x(b)
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
  def contrazip[A, B](e: EncodeJson[A \/ B]): (EncodeJson[A], EncodeJson[B]) =
    (EncodeJson(a => e(a.left)), EncodeJson(b => e(b.right)))

  def fromFoldable[F[_], A](implicit A: EncodeJson[A], F: Foldable[F]): EncodeJson[F[A]] =
    EncodeJson(fa => jArray(F.foldLeft(fa, Nil: List[Json])((list, a) => A.encode(a) :: list).reverse))

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

  implicit def MaybeEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Maybe[A]] = EncodeJson(_.cata(e(_), jNull))

  implicit def DisjunctionEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[A \/ B] =
    EncodeJson(_.fold(
      a => jSingleObject("Left", ea(a)),
      b => jSingleObject("Right", eb(b))
    ))

  implicit def EitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Either[A, B]] =
    EncodeJson(_ match {
      case Left(a)  => jSingleObject("Left", ea(a))
      case Right(b) => jSingleObject("Right", eb(b))
    })

  implicit def ValidationEncodeJson[E, A](implicit ea: EncodeJson[E], eb: EncodeJson[A]): EncodeJson[Validation[E, A]] =
    EncodeJson(_ fold (
      e => jSingleObject("Failure", ea(e)), a => jSingleObject("Success", eb(a))
    ))

  implicit def MapLikeEncodeJson[M[K, +V] <: Map[K, V], K, V](implicit K: EncodeJsonKey[K], e: EncodeJson[V]): EncodeJson[M[K, V]] =
    EncodeJson(x => jObjectAssocList(
      x.toList map {
        case (k, v) => (K.toJsonKey(k), e(v))
      }
    ))

  implicit def IListEncodeJson[A: EncodeJson]: EncodeJson[IList[A]] =
    fromFoldable[IList, A]

  implicit def DListEncodeJson[A: EncodeJson]: EncodeJson[DList[A]] =
    fromFoldable[DList, A]

  implicit def EphemeralStreamEncodeJson[A: EncodeJson]: EncodeJson[EphemeralStream[A]] =
    fromFoldable[EphemeralStream, A]

  implicit def ISetEncodeJson[A: EncodeJson]: EncodeJson[ISet[A]] =
    fromFoldable[ISet, A]

  implicit def NonEmptyListEncodeJson[A: EncodeJson]: EncodeJson[NonEmptyList[A]] =
    fromFoldable[NonEmptyList, A]

  implicit def IMapEncodeJson[A, B](implicit A: EncodeJsonKey[A], B: EncodeJson[B]): EncodeJson[A ==>> B] =
    EncodeJson(x => jObjectAssocList(
      x.foldrWithKey(Nil: List[(String, Json)])(
        (k, v, list) => (A.toJsonKey(k), B(v)) :: list
      )
    ))

  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }
}
