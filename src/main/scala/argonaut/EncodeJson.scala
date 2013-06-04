package argonaut

import scalaz._, Scalaz._, Lens._

import Json._

/**
 * Encode an arbitrary value as a JSON value.
 *
 * @author Tony Morris
 */
trait EncodeJson[-A] {
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
}

trait EncodeJsons extends GeneratedEncodeJsons {
  def contrazip[A, B](e: EncodeJson[A \/ B]): (EncodeJson[A], EncodeJson[B]) =
    (EncodeJson(a => e(a.left)), EncodeJson(b => e(b.right)))

  implicit val IdEncodeJson: EncodeJson[Json] =
    EncodeJson(q => q)

  implicit val HCursorEncodeJson: EncodeJson[HCursor] =
    EncodeJson(q => q.focus)

  implicit val UnitEncodeJson: EncodeJson[Unit] =
    EncodeJson(_ => jEmptyObject)

  implicit def ListEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[List[A]] =
    EncodeJson(a => jArray(a map (e(_))))

  implicit def VectorEncodeJson[A](implicit e: EncodeJson[List[A]]): EncodeJson[Vector[A]] =
    EncodeJson(a => e(a.toList))

  implicit def StreamEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Stream[A]] =
    EncodeJson(a => jArray(a.toList map (e(_))))

  implicit val StringEncodeJson: EncodeJson[String] =
    EncodeJson(jString)

  implicit val DoubleEncodeJson: EncodeJson[Double] =
    EncodeJson(jNumber)

  implicit val FloatEncodeJson: EncodeJson[Float] =
    EncodeJson(a => jNumber(a))

  implicit val IntEncodeJson: EncodeJson[Int] =
    EncodeJson(a => jNumber(a.toDouble))

  implicit val LongEncodeJson: EncodeJson[Long] =
    EncodeJson(a => jString(a.toString))

  implicit val BooleanEncodeJson: EncodeJson[Boolean] =
    EncodeJson(jBool)

  implicit val CharEncodeJson: EncodeJson[Char] =
    EncodeJson(a => jString(a.toString))

  implicit val JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    EncodeJson(a => jNumber(a.doubleValue))

  implicit val JFloatEncodeJson: EncodeJson[java.lang.Float] =
    EncodeJson(a => jNumber(a.floatValue.toDouble))

  implicit val JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    EncodeJson(a => jString(a.toString))

  implicit val JLongEncodeJson: EncodeJson[java.lang.Long] =
    EncodeJson(a => jString(a.toString))

  implicit val JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    EncodeJson(a => jBool(a.booleanValue))

  implicit val JCharacterEncodeJson: EncodeJson[java.lang.Character] =
    EncodeJson(a => jString(a.toString))

  implicit def OptionEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Option[A]] =
    EncodeJson(_ match {
      case None => jNull
      case Some(a) => e(a)
    })

  implicit def ScalazEitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[A \/ B] =
    EncodeJson(_.fold(
      a => jSingleObject("Left", ea(a)),
      b => jSingleObject("Right", eb(b))
    ))

  implicit def EitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Either[A, B]] =
    EncodeJson(_ match {
      case Left(a) => jSingleObject("Left", ea(a))
      case Right(b) => jSingleObject("Right", eb(b))
    })

  implicit def ValidationEncodeJson[E, A](implicit ea: EncodeJson[E], eb: EncodeJson[A]): EncodeJson[Validation[E, A]] =
    EncodeJson(_ fold (
      e => jSingleObject("Failure", ea(e))
    , a => jSingleObject("Success", eb(a))
    ))

  implicit def MapEncodeJson[V](implicit e: EncodeJson[V]): EncodeJson[Map[String, V]] =
    EncodeJson(x => jObjectAssocList(
      x.toList map {
        case (k, v) => (k, e(v))
      }
    ))

  implicit def SetEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Set[A]] =
    EncodeJson(ListEncodeJson[A] contramap ((_: Set[A]).toList) apply _)

  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }

}
