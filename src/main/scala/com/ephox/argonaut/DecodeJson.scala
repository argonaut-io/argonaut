package com.ephox
package argonaut

import scalaz._, Scalaz._, LensT._

/**
 * The result of decoding a JSON value to a value of the type `A`, which may not succeed.
 *
 * @author Tony Morris
 */
sealed trait DecodeResult[+A] {
  /**
   * Return the JSON value the caused a decoding failure.
   */
  def json: Option[Json] =
    this match {
      case DecodeError(j, _) => Some(j)
      case DecodeValue(_) => None
    }

  /**
   * Return the decoding failure message or the decoded value.
   */
  def toEither: Either[String, A] =
    this match {
      case DecodeError(_, e) => Left(e)
      case DecodeValue(a) => Right(a)
    }

  /**
   * Return the decoding failure message or the decoded value.
   */
  def toValidation: Validation[String, A] =
    this match {
      case DecodeError(_, e) => Failure(e)
      case DecodeValue(a) => Success(a)
    }

  /**
   * Return the decoded value.
   */
  def value: Option[A] =
    toEither.right.toOption

  /**
   * Return the error message.
   */
  def error: Option[String] =
    toEither.left.toOption

  /**
   * Returns either a pretty-printed English message or a decoded value.
   */
  def run: Either[String, A] =
    this match {
      case DecodeError(j, e) => Left("Expected: \"" + e + "\" Actual \"" + j.name + "\"")
      case DecodeValue(a) => Right(a)
    }

  /**
   * Covariant functor.
   */
  def map[B](f: A => B): DecodeResult[B] =
    this match {
      case DecodeError(j, e) => DecodeError(j, e)
      case DecodeValue(a) => DecodeValue(f(a))
    }

  /**
   * Monad.
   */
  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] =
    this match {
      case DecodeError(j, e) => DecodeError(j, e)
      case DecodeValue(a) => f(a)
    }

}
private case class DecodeError[+A](j: Json, s: String) extends DecodeResult[A]
private case class DecodeValue[+A](a: A) extends DecodeResult[A]

object DecodeResult extends DecodeResults {
  def apply[A](a: A): DecodeResult[A] =
    DecodeValue(a)
}

trait DecodeResults {
  /**
   * Constructor an error result.
   */
  def decodeError[A](j: Json, s: String): DecodeResult[A] =
    DecodeError(j, s)

  /**
   * The decoding error partial lens.
   */
  def decodeErrorL[A]: DecodeResult[A] @?> (Json, String) =
    PLens {
      case DecodeError(j, e) => Some(Store(q => DecodeError(q._1, q._2), (j, e)))
      case DecodeValue(_) => None
    }

  /**
   * The decoding error partial lens to the JSON value.
   */
  def decodeErrorJL[A]: DecodeResult[A] @?> Json =
    decodeErrorL >=> ~firstLens

  /**
   * The decoding error partial lens to the error message.
   */
  def decodeErrorSL[A]: DecodeResult[A] @?> String =
    decodeErrorL >=> ~secondLens

  /**
   * The decoding value partial lens.
   */
  def decodeValueL[A]: DecodeResult[A] @?> A =
    PLens(_.value map (a => Store(DecodeValue(_), a)))

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def point[A](a: => A) = DecodeValue(a)
    def bind[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }
}

/**
 * Decodes a JSON value to an arbitrary value (may not succeed).
 *
 * @author Tony Morris
 */
trait DecodeJson[+A] {
  import DecodeJson._
  import JsonNumber._

  def apply(j: Json): DecodeResult[A]

  /**
   * Covariant functor.
   */
  def map[B](f: A => B): DecodeJson[B] =
    DecodeJson(apply(_) map f)

  /**
   * Monad.
   */
  def flatMap[B](f: A => DecodeJson[B]): DecodeJson[B] =
    DecodeJson(j => apply(j) flatMap (f(_)(j)))

  /**
   * Isomorphism to kleisli.
   */
  def kleisli: Kleisli[DecodeResult, Json, A] =
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
    DecodeJson(j => apply(j) match {
      case DecodeError(_, _) => x(j)
      case r@DecodeValue(_) => r
    })

  /**
   * Run one or another decoder.
   */
  def split[B](x: DecodeJson[B]): Either[Json, Json] => DecodeResult[Either[A, B]] = {
    case Left(j) => this(j) map (Left(_))
    case Right(j) => x(j) map (Right(_))
  }

  /**
   * Run two decoders.
   */
  def product[B](x: DecodeJson[B]): (Json, Json) => DecodeResult[(A, B)] = {
    case (j1, j2) => for {
      a <- this(j1)
      b <- x(j2)
    } yield (a, b)
  }
}

object DecodeJson extends DecodeJsons {
  def apply[A](f: Json => DecodeResult[A]): DecodeJson[A] =
    new DecodeJson[A] {
      def apply(j: Json) = f(j)
    }
}

trait DecodeJsons {
  import JsonIdentity._
  import DecodeResult._

  /**
   * Construct a decoder that if fails, uses the given error message.
   */
  def decodej[A](f: Json => Option[A], n: String): DecodeJson[A] =
    DecodeJson(j =>
      f(j) match {
        case None => decodeError(j, n)
        case Some(a) => DecodeResult(a)
      }
    )

  /**
   * Construct a succeeding decoder from the given function.
   */
  def decodeArr[A](f: Json => A): DecodeJson[A] =
    DecodeJson(j => DecodeResult(f(j)))

  implicit def IdDecodeJson: DecodeJson[Json] =
    decodeArr(q => q)

  implicit def ListDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[List[A]] =
    DecodeJson(j =>
      j.array match {
        case None => decodeError(j, "[A]List[A]")
        case Some(js) => js.traverse[DecodeResult, A](e(_))
      }
    )

  implicit def StreamDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Stream[A]] =
    DecodeJson(j =>
      j.array match {
        case None => decodeError(j, "[A]Stream[A]")
        case Some(js) => js.toStream.traverse[DecodeResult, A](e(_))
      }
    )

  implicit def StringDecodeJson: DecodeJson[String] =
    decodej(_.string, "String")

  implicit def DoubleDecodeJson: DecodeJson[Double] =
    decodej(x => if(x.isNull) Some(Double.NaN) else x.number map (_.toDouble), "Double")

  implicit def FloatDecodeJson: DecodeJson[Float] =
    decodej(x => if(x.isNull) Some(Float.NaN) else x.number map (_.toFloat), "Float")

  implicit def IntDecodeJson: DecodeJson[Int] =
    decodej(_.string flatMap (s => try { Some(s.toInt) } catch { case _ => None }), "Int")

  implicit def LongDecodeJson: DecodeJson[Long] =
    decodej(_.string flatMap (s => try { Some(s.toLong) } catch { case _ => None }), "Long")

  implicit def BooleanDecodeJson: DecodeJson[Boolean] =
    decodej(_.bool, "Boolean")

  implicit def CharDecodeJson: DecodeJson[Char] =
    decodej(_.string flatMap (s => if(s.length == 1) Some(s(0)) else None), "Char")

  implicit def JDoubleDecodeJson: DecodeJson[java.lang.Double] =
    decodej(_.number map (_.toDouble), "java.lang.Double")

  implicit def JFloatDecodeJson: DecodeJson[java.lang.Float] =
    decodej(_.number map (_.toFloat), "java.lang.Float")

  implicit def JIntegerDecodeJson: DecodeJson[java.lang.Integer] =
    decodej(_.string flatMap (s => try { Some(s.toInt) } catch { case _ => None }), "java.lang.Integer")

  implicit def JLongDecodeJson: DecodeJson[java.lang.Long] =
    decodej(_.string flatMap (s => try { Some(s.toLong) } catch { case _ => None }), "java.lang.Long")

  implicit def JBooleanDecodeJson: DecodeJson[java.lang.Boolean] =
    decodej(_.bool map (q => q), "java.lang.Boolean")

  implicit def JCharacterDecodeJson: DecodeJson[java.lang.Character] =
    decodej(_.string flatMap (s => if(s == 1) Some(s(0)) else None), "java.lang.Character")

  implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] =
    DecodeJson(j =>
      if(j.isNull)
        DecodeResult(None)
      else
        e(j) map (Some(_))
    )

  implicit def EitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Either[A, B]] =
    DecodeJson(j =>
      j.obj match {
        case None => decodeError(j, "[A, B]Either[A, B]")
        case Some(o) => o.toList match {
          case ("Left", v) :: Nil => ea(v) map (Left(_))
          case ("Right", v) :: Nil => eb(v) map (Right(_))
          case _ => decodeError(j, "[A, B]Either[A, B]")
        }
      })

  implicit def ValidationDecodeJson[E, A](implicit ea: DecodeJson[E], eb: DecodeJson[A]): DecodeJson[Validation[E, A]] =
    DecodeJson(j =>
      j.obj match {
        case None => decodeError(j, "[E, A]Validation[E, A]")
        case Some(o) => o.toList match {
          case ("Failure", v) :: Nil => ea(v) map (Validation.failure(_))
          case ("Success", v) :: Nil => eb(v) map (Validation.success(_))
          case _ => decodeError(j, "[E, A]Validation[E, A]")
        }
      })

  implicit def MapDecodeJson[V](implicit e: DecodeJson[V]): DecodeJson[Map[String, V]] =
    DecodeJson(j =>
      j.obj match {
        case None => decodeError(j, "[V]Map[String, V]")
        case Some(js) => js.toList.traverse[DecodeResult, (String, V)]{
          case (k, v) => e(v) map ((k, _))
        } map (_.toMap)
      }
    )

  implicit def SetDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Set[A]] =
    DecodeJson(j =>
      j.array match {
        case None => decodeError(j, "[A]Set[A]")
        case Some(js) => js.toSet.traverse[DecodeResult, A](e(_))
      }
    )

  implicit def Tuple2DecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[(A, B)] =
    DecodeJson(j =>
      j.array match {
        case Some(a::b::Nil) => for {
          aa <- ea(a)
          bb <- eb(b)
        } yield (aa, bb)
        case _ => decodeError(j, "[A, B](A, B)")
      })

  implicit def Tuple3DecodeJson[A, B, C](implicit ea: DecodeJson[A], eb: DecodeJson[B], ec: DecodeJson[C]): DecodeJson[(A, B, C)] =
    DecodeJson(j =>
      j.array match {
        case Some(a::b::c::Nil) => for {
          aa <- ea(a)
          bb <- eb(b)
          cc <- ec(c)
        } yield (aa, bb, cc)
        case _ => decodeError(j, "[A, B, C](A, B, C)")
      })

  implicit def DecodeJsonMonad: Monad[DecodeJson] = new Monad[DecodeJson] {
    def point[A](a: => A) = DecodeJson(_ => DecodeValue(a))
    def bind[A, B](a: DecodeJson[A])(f: A => DecodeJson[B]) = a flatMap f
    override def map[A, B](a: DecodeJson[A])(f: A => B) = a map f
  }

}
