package argonaut

import Json._
import JsonIdentity._
import EncodeJsonNumber._

/**
 * Encode an arbitrary value as a JSON value.
 *
 * @author Tony Morris
 */
trait EncodeJson[A] {
  /**
   * Encode the given value. Alias for `encode`.
   */
  def apply(a: A): Json = encode(a)

  /**
   * Encode the given value.
   */
  def encode(a: A): Json

  /**
   * Contravariant functor.
   */
  def contramap[B](f: B => A): EncodeJson[B] = {
    EncodeJson(b => apply(f(b)))
  }

  /**
   * Transform the resulting Json instance.
   */
  def mapJson(f: Json => Json): EncodeJson[A] = {
    val original = this
    (a: A) => f(original(a))
  }

  /**
   * Split on this encoder and the given encoder.
   */
  def <&>[B](x: => EncodeJson[B]): EncodeJson[Either[A, B]] = {
    EncodeJson {
      case Left(a) => apply(a)
      case Right(b) => x(b)
    }
  }
}

object EncodeJson extends EncodeJsons with EncodeJsonMacro {
  def apply[A](f: A => Json): EncodeJson[A] =
    (a: A) => f(a)

  def of[A: EncodeJson]: EncodeJson[A] = implicitly[EncodeJson[A]]
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

  implicit def ListEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[List[A]] =
    EncodeJson(a => jArray(a.map(e(_))))

  implicit def VectorEncodeJson[A](implicit e: EncodeJson[List[A]]): EncodeJson[Vector[A]] =
    EncodeJson(a => e(a.toList))

  implicit def StreamEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Stream[A]] =
    EncodeJson(a => jArray(a.toList.map(e(_))))

  implicit val StringEncodeJson: EncodeJson[String] =
    EncodeJson(jString)

  implicit val UUIDEncodeJson: EncodeJson[java.util.UUID] =
    StringEncodeJson.contramap(_.toString)

  implicit val DoubleEncodeJson: EncodeJson[Double] =
    EncodeJson(a => a.asPossibleJsonNumber.fold(jNull)(_.asJson))

  implicit val FloatEncodeJson: EncodeJson[Float] =
    EncodeJson(a => a.asPossibleJsonNumber.fold(jNull)(_.asJson))

  implicit val IntEncodeJson: EncodeJson[Int] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val LongEncodeJson: EncodeJson[Long] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val ShortEncodeJson: EncodeJson[Short] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val ByteEncodeJson: EncodeJson[Byte] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val BigDecimalEncodeJson: EncodeJson[BigDecimal] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val BigIntEncodeJson: EncodeJson[BigInt] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val BooleanEncodeJson: EncodeJson[Boolean] =
    EncodeJson(jBool)

  implicit val CharEncodeJson: EncodeJson[Char] =
    EncodeJson(a => jString(a.toString))

  implicit val JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    EncodeJson(a => a.asPossibleJsonNumber.fold(jNull)(_.asJson))

  implicit val JFloatEncodeJson: EncodeJson[java.lang.Float] =
    EncodeJson(a => a.asPossibleJsonNumber.fold(jNull)(_.asJson))

  implicit val JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val JLongEncodeJson: EncodeJson[java.lang.Long] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val JShortEncodeJson: EncodeJson[java.lang.Short] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val JByteEncodeJson: EncodeJson[java.lang.Byte] =
    EncodeJson(a => a.asJsonNumber.asJson)

  implicit val JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    EncodeJson(a => jBool(a.booleanValue))

  implicit val JCharacterEncodeJson: EncodeJson[java.lang.Character] =
    EncodeJson(a => jString(a.toString))

  implicit def OptionEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Option[A]] = {
    EncodeJson(_ match {
      case None    => jNull
      case Some(a) => e(a)
    })
  }

  implicit def EitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Either[A, B]] = {
    EncodeJson(_ match {
      case Left(a)  => jSingleObject("Left", ea(a))
      case Right(b) => jSingleObject("Right", eb(b))
    })
  }

  implicit def MapEncodeJson[K, V](implicit K: EncodeJsonKey[K], e: EncodeJson[V]): EncodeJson[Map[K, V]] = {
    EncodeJson(x => jObjectAssocList(
      x.toList.map{
        case (k, v) => (K.toJsonKey(k), e(v))
      }
    ))
  }

  implicit def SetEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Set[A]] = {
    EncodeJson(ListEncodeJson[A].contramap((_: Set[A]).toList).apply(_))
  }
}
