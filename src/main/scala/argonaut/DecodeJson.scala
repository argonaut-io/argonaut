package argonaut

import scala.util.control.Exception.catching
import scalaz._, Scalaz._
import Json._

sealed trait DecodeJson[+A] {
  def apply(c: HCursor): DecodeResult[A]

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
    DecodeJson(c => if (f(c)) apply(c) else DecodeResult.fail(message, c.history))

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
    DecodeJson(c => {
      val q = apply(c)
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
      def apply(c: HCursor) =
        r(c)
    }
}

trait DecodeJsons {
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

  implicit def IdDecodeJson: DecodeJson[HCursor] =
    decodeArr(q => q)

  implicit def JsonDecodeJson: DecodeJson[Json] =
    DecodeJson(j => DecodeResult.ok(j.focus))

  implicit def ListDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[List[A]] =
    DecodeJson(a =>
      a.downArray.hcursor match {
        case None =>
          if (a.focus.isArray)
            DecodeResult.ok(Nil)
          else
            DecodeResult.fail("[A]List[A]", a.history)
        case Some(hcursor) =>
          hcursor.traverseDecode(List[A]())(_.right, (acc, c) =>
            c.jdecode[A] map (_ :: acc)) map (_.reverse)
      })

  implicit def VectorDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Vector[A]] =
    DecodeJson(a =>
      a.downArray.hcursor match {
        case None =>
          if (a.focus.isArray)
            DecodeResult.ok(Vector[A]())
          else
            DecodeResult.fail("[A]List[A]", a.history)
        case Some(hcursor) =>
          hcursor.traverseDecode(Vector[A]())(_.right, (acc, c) =>
            c.jdecode[A] map (acc :+ _))
      })

  implicit def StreamDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Stream[A]] =
    DecodeJson(a =>
      a.downArray.hcursor match {
        case None =>
          if (a.focus.isArray)
            DecodeResult.ok(Stream[A]())
          else
            DecodeResult.fail("[A]List[A]", a.history)
        case Some(hcursor) =>
          hcursor.traverseDecode(Stream[A]())(_.right, (acc, c) =>
            c.jdecode[A] map (_ #:: acc)) map (_.reverse)
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
      (x.number map (_.toInt)).orElse(
      (x.string flatMap (s => tryTo(s.toInt)))), "Int")

  implicit def LongDecodeJson: DecodeJson[Long] =
    optionDecoder(x =>
      (x.number map (_.toLong)).orElse(
      (x.string flatMap (s => tryTo(s.toLong)))), "Long")

  implicit def BooleanDecodeJson: DecodeJson[Boolean] =
    optionDecoder(_.bool, "Boolean")

  implicit def CharDecodeJson: DecodeJson[Char] =
    optionDecoder(_.string flatMap (s => if(s.length == 1) Some(s(0)) else None), "Char")

  implicit def JDoubleDecodeJson: DecodeJson[java.lang.Double] =
    optionDecoder(_.number map (_.toDouble), "java.lang.Double")

  implicit def JFloatDecodeJson: DecodeJson[java.lang.Float] =
    optionDecoder(_.number map (_.toFloat), "java.lang.Float")

  implicit def JIntegerDecodeJson: DecodeJson[java.lang.Integer] =
    optionDecoder(_.string flatMap (s => tryTo(s.toInt)), "java.lang.Integer")

  implicit def JLongDecodeJson: DecodeJson[java.lang.Long] =
    optionDecoder(_.string flatMap (s => tryTo(s.toLong)), "java.lang.Long")

  implicit def JBooleanDecodeJson: DecodeJson[java.lang.Boolean] =
    optionDecoder(_.bool map (q => q), "java.lang.Boolean")

  implicit def JCharacterDecodeJson: DecodeJson[java.lang.Character] =
    optionDecoder(_.string flatMap (s => if(s == 1) Some(s(0)) else None), "java.lang.Character")

  implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] =
    DecodeJson(a => if (a.focus.isNull)
      DecodeResult.ok(None)
    else
      e(a).option
    )

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

  implicit def Tuple2DecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[(A, B)] =
    DecodeJson(c =>
      c.jdecode[List[HCursor]] flatMap {
        case ca :: cb :: Nil => for {
            xa <- ea(ca)
            xb <- eb(cb)
          } yield (xa, xb)
        case _ => DecodeResult.fail("[A, B]Tuple2[A, B]", c.history)
      })

  implicit def Tuple3DecodeJson[A, B, C](implicit ea: DecodeJson[A], eb: DecodeJson[B], ec: DecodeJson[C]): DecodeJson[(A, B, C)] =
    DecodeJson(c =>
      c.jdecode[List[HCursor]] flatMap {
        case ca :: cb :: cc :: Nil => for {
            xa <- ea(ca)
            xb <- eb(cb)
            xc <- ec(cc)
          } yield (xa, xb, xc)
        case x =>
          DecodeResult.fail("[A, B, C]Tuple3[A, B, C]", c.history)
      })

  implicit def Tuple4DecodeJson[A, B, C, D](implicit ea: DecodeJson[A], eb: DecodeJson[B], ec: DecodeJson[C], ed: DecodeJson[D]): DecodeJson[(A, B, C, D)] =
    DecodeJson(c =>
      c.jdecode[List[HCursor]] flatMap {
        case ca :: cb :: cc :: cd :: Nil => for {
          xa <- ea(ca)
          xb <- eb(cb)
          xc <- ec(cc)
          xd <- ed(cd)
        } yield (xa, xb, xc, xd)
        case _ => DecodeResult.fail("[A, B, C, D]Tuple4[A, B, C, D]", c.history)
      })

  def jdecode1[A: DecodeJson, X](f: A => X): DecodeJson[X] =
    implicitly[DecodeJson[A]].map(f)

  def jdecode2[A: DecodeJson, B: DecodeJson, X](f: (A, B) => X): DecodeJson[X] =
    implicitly[DecodeJson[(A, B)]].map(x => f(x._1, x._2))

  def jdecode3[A: DecodeJson, B: DecodeJson, C: DecodeJson, X](f: (A, B, C) => X): DecodeJson[X] =
    implicitly[DecodeJson[(A, B, C)]].map(x => f(x._1, x._2, x._3))

  def jdecode4[A: DecodeJson, B: DecodeJson, C: DecodeJson, D: DecodeJson, X](f: (A, B, C, D) => X): DecodeJson[X] =
    implicitly[DecodeJson[(A, B, C, D)]].map(x => f(x._1, x._2, x._3, x._4))

  def jdecode1L[A: DecodeJson, X](f: A => X)(an: JsonString): DecodeJson[X] =
    DecodeJson(x => for {
      aa <- x.get[A](an)
    } yield f(aa))

  def jdecode2L[A: DecodeJson, B: DecodeJson, X](f: (A, B) => X)(an: JsonString, bn: JsonString): DecodeJson[X] =
    DecodeJson(x => for {
      aa <- x.get[A](an)
      bb <- x.get[B](bn)
    } yield f(aa, bb))

  def jdecode3L[A: DecodeJson, B: DecodeJson, C: DecodeJson, X](f: (A, B, C) => X)(an: JsonString, bn: JsonString, cn: JsonString): DecodeJson[X] =
    DecodeJson(x => for {
      aa <- x.get[A](an)
      bb <- x.get[B](bn)
      cc <- x.get[C](cn)
    } yield f(aa, bb, cc))

  def jdecode4L[A: DecodeJson, B: DecodeJson, C: DecodeJson, D: DecodeJson, X](f: (A, B, C, D) => X)(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString): DecodeJson[X] =
    DecodeJson(x => for {
      aa <- x.get[A](an)
      bb <- x.get[B](bn)
      cc <- x.get[C](cn)
      dd <- x.get[D](dn)
    } yield f(aa, bb, cc, dd))

  def jdecode5L[A: DecodeJson, B: DecodeJson, C: DecodeJson, D: DecodeJson, E: DecodeJson, X](f: (A, B, C, D, E) => X)(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString, en: JsonString): DecodeJson[X] =
    DecodeJson(x => for {
      aa <- x.get[A](an)
      bb <- x.get[B](bn)
      cc <- x.get[C](cn)
      dd <- x.get[D](dn)
      ee <- x.get[E](en)
    } yield f(aa, bb, cc, dd, ee))


}
