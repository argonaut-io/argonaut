package argonaut

import scalaz._, Scalaz._, Lens._

import Json._

/**
 * Encode an arbitrary value as a JSON value.
 *
 * @author Tony Morris
 */
sealed trait EncodeJson[-A] {
  /**
   * Encode the given value.
   */
  def apply(a: A): Json

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
      def apply(a: A) = f(a)
    }
}

trait EncodeJsons {
  def contrazip[A, B](e: EncodeJson[A \/ B]): (EncodeJson[A], EncodeJson[B]) =
    (EncodeJson(a => e(a.left)), EncodeJson(b => e(b.right)))

  implicit val IdEncodeJson: EncodeJson[Json] =
    EncodeJson(q => q)

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
    EncodeJson(a => jString(a.toString))

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

  implicit def Tuple1EncodeJson[A](implicit ea: EncodeJson[A]): EncodeJson[Tuple1[A]] =
    EncodeJson(a => jArray(List(ea(a._1))))

  implicit def Tuple2EncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[(A, B)] =
    EncodeJson({
      case (a, b) => jArray(List(ea(a), eb(b)))
    })

  implicit def Tuple3EncodeJson[A, B, C](implicit ea: EncodeJson[A], eb: EncodeJson[B], ec: EncodeJson[C]): EncodeJson[(A, B, C)] =
    EncodeJson({
      case (a, b, c) => jArray(List(ea(a), eb(b), ec(c)))
    })

  implicit def Tuple4EncodeJson[A, B, C, D](implicit ea: EncodeJson[A], eb: EncodeJson[B], ec: EncodeJson[C], ed: EncodeJson[D]): EncodeJson[(A, B, C, D)] =
    EncodeJson({
      case (a, b, c, d) => jArray(List(ea(a), eb(b), ec(c), ed(d)))
    })


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

  def jencode1L[X, A: EncodeJson](f: X => A)(an: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val a = f(x)
      List((an, implicitly[EncodeJson[A]] apply a))
    }))

  def jencode2L[X, A: EncodeJson, B: EncodeJson](f: X => (A, B))(an: JsonString, bn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b))
    }))

  def jencode3L[X, A: EncodeJson, B: EncodeJson, C: EncodeJson](f: X => (A, B, C))(an: JsonString, bn: JsonString, cn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b, c) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b), (cn, implicitly[EncodeJson[C]] apply c))
    }))

  def jencode4L[X, A: EncodeJson, B: EncodeJson, C: EncodeJson, D: EncodeJson](f: X => (A, B, C, D))(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b, c, d) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b), (cn, implicitly[EncodeJson[C]] apply c), (dn, implicitly[EncodeJson[D]] apply d))
    }))

  def jencode5L[X, A: EncodeJson, B: EncodeJson, C: EncodeJson, D: EncodeJson, E: EncodeJson](f: X => (A, B, C, D, E))(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString, en: JsonString): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (a, b, c, d, e) = f(x)
      List((an, implicitly[EncodeJson[A]] apply a), (bn, implicitly[EncodeJson[B]] apply b), (cn, implicitly[EncodeJson[C]] apply c), (dn, implicitly[EncodeJson[D]] apply d), (en, implicitly[EncodeJson[E]] apply e))
    }))

}
