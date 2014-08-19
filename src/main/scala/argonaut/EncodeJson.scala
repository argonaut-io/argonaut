package argonaut

import scalaz.{ Coproduct => _, _}, Scalaz._
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

  /* ==== shapeless for profit ==== */

  import shapeless._

  def derive[A](implicit ev: LabelledTypeClass[EncodeJson]): EncodeJson[A] =
    macro GenericMacros.deriveLabelledInstance[EncodeJson, A]

  object auto {
    implicit def AutoEncodeJson[A](implicit ev: LabelledTypeClass[EncodeJson]): EncodeJson[A] =
      macro GenericMacros.deriveLabelledInstance[EncodeJson, A]
  }

  implicit def EncodeJsonTypeClass: LabelledTypeClass[EncodeJson] = new LabelledTypeClass[EncodeJson] {
    def emptyCoproduct =
      EncodeJson(_ => jEmptyObject)

    def coproduct[L, R <: Coproduct](name: String, CL: => EncodeJson[L], CR: => EncodeJson[R]): EncodeJson[L :+: R] =
      EncodeJson(a => a match {
        case Inl(x) => Json((name -> CL.encode(x)))
        case Inr(t) => CR.encode(t)
      })

    def emptyProduct =
      EncodeJson(_ => jEmptyObject)

    def product[A, T <: HList](name: String, A: EncodeJson[A], T: EncodeJson[T]) =
      EncodeJson(a => (name -> A.encode(a.head)) ->: T.encode(a.tail))

    def project[F, G](instance: => EncodeJson[G], to : F => G, from : G => F) =
      instance.contramap(to)
  }

  def of[A: EncodeJson] =
    implicitly[EncodeJson[A]]

}

trait EncodeJsons extends GeneratedEncodeJsons with internal.MacrosCompat {
  def contrazip[A, B](e: EncodeJson[A \/ B]): (EncodeJson[A], EncodeJson[B]) =
    (EncodeJson(a => e(a.left)), EncodeJson(b => e(b.right)))

  implicit val JsonEncodeJson: EncodeJson[Json] =
    EncodeJson(q => q)

  implicit val HCursorEncodeJson: EncodeJson[HCursor] =
    EncodeJson(q => q.focus)

  implicit val UnitEncodeJson: EncodeJson[Unit] =
    EncodeJson(_ => jEmptyObject)

  implicit def TraversableOnceEncodeJson[A, C[_]](implicit e: EncodeJson[A], is: collection.generic.IsTraversableOnce[C[A]]): EncodeJson[C[A]] =
    // cast is necessary as a: is.A, not A
    EncodeJson(a => jArray(is.conversion(a).toList.map(a => e(a.asInstanceOf[A]))))

  implicit val StringEncodeJson: EncodeJson[String] =
    EncodeJson(jString)

  implicit val DoubleEncodeJson: EncodeJson[Double] =
    EncodeJson(n => jNumber(JsonNumberDouble(n)))

  implicit val FloatEncodeJson: EncodeJson[Float] =
    EncodeJson(a => jNumber(JsonNumberDouble(a)))

  implicit val IntEncodeJson: EncodeJson[Int] =
    EncodeJson(a => jNumber(JsonNumberDouble(a.toDouble)))

  implicit val LongEncodeJson: EncodeJson[Long] =
    EncodeJson(a => jString(a.toString))

  implicit val ShortEncodeJson: EncodeJson[Short] =
    EncodeJson(a => jString(a.toString))

  implicit val BooleanEncodeJson: EncodeJson[Boolean] =
    EncodeJson(jBool)

  implicit val CharEncodeJson: EncodeJson[Char] =
    EncodeJson(a => jString(a.toString))

  implicit val JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    EncodeJson(a => jNumber(JsonNumberDouble(a.doubleValue)))

  implicit val JFloatEncodeJson: EncodeJson[java.lang.Float] =
    EncodeJson(a => jNumber(JsonNumberDouble(a.floatValue.toDouble)))

  implicit val JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    EncodeJson(a => jString(a.toString))

  implicit val JLongEncodeJson: EncodeJson[java.lang.Long] =
    EncodeJson(a => jString(a.toString))

  implicit val JShortEncodeJson: EncodeJson[java.lang.Short] =
    EncodeJson(a => jString(a.toString))

  implicit val JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    EncodeJson(a => jBool(a.booleanValue))

  implicit val JCharacterEncodeJson: EncodeJson[java.lang.Character] =
    EncodeJson(a => jString(a.toString))

  implicit def OptionEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Option[A]] =
    EncodeJson(_ match {
      case None    => jNull
      case Some(a) => e(a)
    })

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

  implicit def MapEncodeJson[V](implicit e: EncodeJson[V]): EncodeJson[Map[String, V]] =
    EncodeJson(x => jObjectAssocList(
      x.toList map {
        case (k, v) => (k, e(v))
      }
    ))

  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }
}
