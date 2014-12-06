package argonaut

import scala.math.{ Ordering => ScalaOrdering }
import scala.collection.immutable.SortedSet
import scala.util.control.Exception.catching
import scalaz._, Scalaz._
import Json._

trait DecodeJson[A] {
  /**
   * Decode the given hcursor. Alias for `decode`.
   */
  def apply(c: HCursor): DecodeResult[A] =
    decode(c)

  /**
   * Decode the given hcursor.
   */
  def decode(c: HCursor): DecodeResult[A]

  /**
   * Decode the given acursor.
   */
  def tryDecode(c: ACursor): DecodeResult[A] = c.either match {
    case -\/(invalid) => DecodeResult.fail("Attempt to decode value on failed cursor.", invalid.history)
    case \/-(valid) => decode(valid)
  }

  /**
   * Decode the given json.
   */
  def decodeJson(j: Json): DecodeResult[A] =
    decode(j.hcursor)

  /**
   * Covariant functor.
   */
  def map[B](f: A => B): DecodeJson[B] =
    DecodeJson(apply(_) map f)

  /**
   * Monad.
   */
  def flatMap[B](f: A => DecodeJson[B]): DecodeJson[B] =
    DecodeJson(c => apply(c) flatMap (f(_)(c)))

  /**
   * Build a new DecodeJson codec with the specified name.
   */
  def setName(n: String): DecodeJson[A] =
    DecodeJson(c => apply(c).result.fold(
      { case (_, h) => DecodeResult.fail(n, h) },
      a => DecodeResult.ok(a)
    ))

  /**
   * Build a new DecodeJson codec with the specified precondition that f(c) == true.
   */
  def validate(f: HCursor => Boolean, message: => String) =
    DecodeJson(c => if (f(c)) apply(c) else DecodeResult.fail[A](message, c.history))

  /**
   * Build a new DecodeJson codec with the precondition that the cursor focus is object with exactly n field.
   */
  def validateFields(n: Int) =
    validate(_.focus.obj exists (_.size == n), "Expected json object with exactly [" + n + "] fields.")

  /**
   * Isomorphism to kleisli.
   */
  def kleisli: Kleisli[DecodeResult, HCursor, A] =
    Kleisli(apply(_))

  /**
   * Combine two decoders.
   */
  def &&&[B](x: DecodeJson[B]): DecodeJson[(A, B)] =
    DecodeJson(j => for {
      a <- this(j)
      b <- x(j)
    } yield (a, b))

  /**
   * Choose the first succeeding decoder.
   */
  def |||[AA >: A](x: => DecodeJson[AA]): DecodeJson[AA] =
    DecodeJson[AA](c => {
      val q = apply(c).map(a => a: AA)
      q.result.fold(_ => x(c), _ => q)
    })

  /**
   * Run one or another decoder.
   */
  def split[B](x: DecodeJson[B]): HCursor \/ HCursor => DecodeResult[A \/ B] =
    c => c.fold(a => this(a) map (_.left), a => x(a) map (_.right))

  /**
   * Run two decoders.
   */
  def product[B](x: DecodeJson[B]): (HCursor, HCursor) => DecodeResult[(A, B)] = {
    case (a1, a2) => for {
      a <- this(a1)
      b <- x(a2)
    } yield (a, b)
  }
}

object DecodeJson extends DecodeJsons {
  def apply[A](r: HCursor => DecodeResult[A]): DecodeJson[A] =
    new DecodeJson[A] {
      def decode(c: HCursor) =
        r(c)
    }

  def withReattempt[A](r: ACursor => DecodeResult[A]): DecodeJson[A] =
    new DecodeJson[A] {
      def decode(c: HCursor): DecodeResult[A] =
        tryDecode(c.acursor)

      override def tryDecode(c: ACursor) =
        r(c)
    }

  /* ==== shapeless for profit ==== */

  import shapeless._

  def derive[A](implicit ev: LabelledTypeClass[DecodeJson]): DecodeJson[A] =
    macro GenericMacros.deriveLabelledInstance[DecodeJson, A]

  object auto {
    implicit def AutoDecodeJson[A](implicit ev: LabelledTypeClass[DecodeJson]): DecodeJson[A] =
      macro GenericMacros.deriveLabelledInstance[DecodeJson, A]
  }

  implicit def DecodeJsonTypeClass: LabelledTypeClass[DecodeJson] = new LabelledTypeClass[DecodeJson] {
    def emptyCoproduct =
      DecodeJson(c =>
        DecodeResult.fail("CNil", c.history)
      )

    def coproduct[L, R <: Coproduct](name: String, CL: => DecodeJson[L], CR: => DecodeJson[R]): DecodeJson[L :+: R] =
      DecodeJson { c =>
        (c --\ name).focus.fold[DecodeResult[L :+: R]](
          CR.decode(c).map(Inr(_))
        )(aJson => aJson.as(CL).map(Inl(_)))
      }

    def emptyProduct =
      DecodeJson(c =>
        c.focus.obj.filter(_.isEmpty).fold[DecodeResult[HNil]](
          DecodeResult.fail("HNil", c.history)
        )(_ => (HNil: HNil).point[DecodeResult])
      )

    def product[A, T <: HList](name: String, A: DecodeJson[A], T: DecodeJson[T]) =
      DecodeJson { c =>
        val aJson = c --\ name
        (aJson.as(A) |@| aJson.delete.as(T))(_ :: _)
      }

    def project[F, G](instance: => DecodeJson[G], to : F => G, from : G => F) =
      instance.map(from)
  }

  def of[A: DecodeJson] =
    implicitly[DecodeJson[A]]
}

trait DecodeJsons extends GeneratedDecodeJsons with internal.MacrosCompat {

  def optionDecoder[A](k: Json => Option[A], e: String): DecodeJson[A] =
    DecodeJson(a => k(a.focus) match {
      case None => DecodeResult.fail(e, a.history)
      case Some(w) => DecodeResult.ok(w)
    })

  /**
   * Construct a succeeding decoder from the given function.
   */
  def decodeArr[A](f: HCursor => A): DecodeJson[A] =
    DecodeJson(j => DecodeResult.ok(f(j)))

  def tryTo[A](f: => A): Option[A] =
    catching(classOf[IllegalArgumentException]).opt(f)

  implicit def HCursorDecodeJson: DecodeJson[HCursor] =
    decodeArr(q => q)

  implicit def JsonDecodeJson: DecodeJson[Json] =
    decodeArr(j => j.focus)

  implicit def CanBuildFronDecodeJson[A, C[_]](implicit e: DecodeJson[A], c: collection.generic.CanBuildFrom[C[_], A, C[A]]): DecodeJson[C[A]] =
    DecodeJson(a =>
      a.downArray.hcursor match {
        case None =>
          if (a.focus.isArray)
            DecodeResult.ok(c.apply.result)
          else
            DecodeResult.fail("[A]List[A]", a.history)
        case Some(hcursor) =>
          hcursor.traverseDecode(c.apply)(_.right, (acc, c) =>
            c.jdecode[A] map (acc += _)).map(_.result)
      })

  implicit def UnitDecodeJson: DecodeJson[Unit] =
    DecodeJson(a => if (a.focus.isNull || a.focus == jEmptyObject || a.focus == jEmptyArray)
        ().point[DecodeResult]
      else
        DecodeResult.fail("Unit", a.history))

  implicit def StringDecodeJson: DecodeJson[String] =
    optionDecoder(_.string, "String")

  implicit def DoubleDecodeJson: DecodeJson[Double] =
    optionDecoder(x => if(x.isNull) Some(Double.NaN) else x.number map (_.toDouble), "Double")

  implicit def FloatDecodeJson: DecodeJson[Float] =
    optionDecoder(x => if(x.isNull) Some(Float.NaN) else x.number map (_.toFloat), "Float")

  implicit def IntDecodeJson: DecodeJson[Int] =
    optionDecoder(x =>
      (x.number map (_.truncateToInt)).orElse(
      (x.string flatMap (s => tryTo(s.toInt)))), "Int")

  implicit def LongDecodeJson: DecodeJson[Long] =
    optionDecoder(x =>
      (x.number map (_.truncateToLong)).orElse(
      (x.string flatMap (s => tryTo(s.toLong)))), "Long")

  implicit def ShortDecodeJson: DecodeJson[Short] =
    optionDecoder(x =>
      (x.number map (_.truncateToShort)).orElse(
      (x.string flatMap (s => tryTo(s.toShort)))), "Short")

  implicit def BigIntDecodeJson: DecodeJson[BigInt] =
    optionDecoder(x =>
      (x.number map (_.truncateToBigInt)).orElse(
      (x.string flatMap (s => tryTo(BigInt(s))))), "BigInt")

  implicit def BigDecimalDecodeJson: DecodeJson[BigDecimal] =
    optionDecoder(x =>
      (x.number map (_.toBigDecimal)).orElse(
      (x.string flatMap (s => tryTo(BigDecimal(s))))), "BigDecimal")

  implicit def BooleanDecodeJson: DecodeJson[Boolean] =
    optionDecoder(_.bool, "Boolean")

  implicit def CharDecodeJson: DecodeJson[Char] =
    optionDecoder(_.string flatMap (s => if(s.length == 1) Some(s(0)) else None), "Char")

  implicit def JDoubleDecodeJson: DecodeJson[java.lang.Double] =
    optionDecoder(_.number map (_.toDouble), "java.lang.Double")

  implicit def JFloatDecodeJson: DecodeJson[java.lang.Float] =
    optionDecoder(_.number map (_.toFloat), "java.lang.Float")

  implicit def JIntegerDecodeJson: DecodeJson[java.lang.Integer] =
    optionDecoder(_.number flatMap (s => tryTo(s.truncateToInt)), "java.lang.Integer")

  implicit def JLongDecodeJson: DecodeJson[java.lang.Long] =
    optionDecoder(_.number flatMap (s => tryTo(s.truncateToLong)), "java.lang.Long")

  implicit def JShortDecodeJson: DecodeJson[java.lang.Short] =
    optionDecoder(_.number flatMap (s => tryTo(s.truncateToShort)), "java.lang.Short")

  implicit def JBooleanDecodeJson: DecodeJson[java.lang.Boolean] =
    optionDecoder(_.bool map (q => q), "java.lang.Boolean")

  implicit def JCharacterDecodeJson: DecodeJson[java.lang.Character] =
    optionDecoder(_.string flatMap (s => if(s.length == 1) Some(s(0)) else None), "java.lang.Character")

  implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] =
    DecodeJson.withReattempt(a => a.success match {
      case None =>
        DecodeResult.ok(None)
      case Some(valid) =>
        if (valid.focus.isNull)
          DecodeResult.ok(None)
        else
          e(valid).option
    })

  implicit def ScalazEitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[A \/ B] =
    implicitly[DecodeJson[Either[A, B]]].map(\/.fromEither(_))

  implicit def EitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Either[A, B]] =
    DecodeJson(a => {
      val l = (a --\ "Left").success
      val r = (a --\ "Right").success
      (l, r) match {
        case (Some(c), None) => ea(c) map (Left(_))
        case (None, Some(c)) => eb(c) map (Right(_))
        case _ => DecodeResult.fail("[A, B]Either[A, B]", a.history)
      }
    })

  implicit def ValidationDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Validation[A, B]] =
    DecodeJson(a => {
      val l = (a --\ "Failure").success
      val r = (a --\ "Success").success
      (l, r) match {
        case (Some(c), None) => ea(c) map (Failure(_))
        case (None, Some(c)) => eb(c) map (Success(_))
        case _ => DecodeResult.fail("[A, B]Validation[A, B]", a.history)
      }
    })

  implicit def MapDecodeJson[V](implicit e: DecodeJson[V]): DecodeJson[Map[String, V]] =
    DecodeJson(a =>
      a.fields match {
        case None => DecodeResult.fail("[V]Map[String, V]", a.history)
        case Some(s) => {
          def spin(x: List[JsonField], m: DecodeResult[Map[String, V]]): DecodeResult[Map[String, V]] =
            x match {
              case Nil => m
              case h::t =>
                spin(t, for {
                    mm <- m
                    v <- a.get(h)(e)
                  } yield mm + ((h, v)))
            }

          spin(s, DecodeResult.ok(Map.empty[String, V]))
        }
      }
    )

  implicit def SetDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Set[A]] =
    implicitly[DecodeJson[List[A]]] map (_.toSet) setName "[A]Set[A]"
}
