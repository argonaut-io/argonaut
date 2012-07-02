package com.ephox
package argonaut

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

  def setName(n: String): DecodeJson[A] =
    DecodeJson(c => apply(c).result match {
      case Left((_, h)) => DecodeResult.failedResult(n, h)
      case Right(a) => DecodeResult(a)
    })


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
      q.result match {
        case Left(_) => x(c)
        case Right(_) => q
      }
    })

  /**
   * Run one or another decoder.
   */
  def split[B](x: DecodeJson[B]): Either[HCursor, HCursor] => DecodeResult[Either[A, B]] = {
    case Left(a) => this(a) map (Left(_))
    case Right(a) => x(a) map (Right(_))
  }

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
      case None => DecodeResult.failedResult(e, a.history)
      case Some(w) => DecodeResult(w)
    })

  /**
   * Construct a succeeding decoder from the given function.
   */
  def decodeArr[A](f: HCursor => A): DecodeJson[A] =
    DecodeJson(j => DecodeResult(f(j)))

  implicit def IdDecodeJson: DecodeJson[HCursor] =
    decodeArr(q => q)

  def ListDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[List[A]] =
    DecodeJson(_.traverseA[DecodeResult[List[A]]](
      Kleisli[({type λ[+α] = State[DecodeResult[List[A]], α]})#λ, HCursor, ACursor](c => {
        State((x: DecodeResult[List[A]]) => (for {
          h <- e(c)
          t <- x
        } yield h :: t, c.right))
      })) exec DecodeResult(Nil)
  )

  def StreamDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Stream[A]] =
    DecodeJson(a =>
      a.traverseA[DecodeResult[Stream[A]]](
        Kleisli[({type λ[+α] = State[DecodeResult[Stream[A]], α]})#λ, HCursor, ACursor](c => {
          State((x: DecodeResult[Stream[A]]) => (for {
              t <- x
              h <- e(c)
            } yield h #:: t, c.right))
        })) exec DecodeResult(Stream.empty)
    )

  implicit def StringDecodeJson: DecodeJson[String] =
    optionDecoder(_.string, "String")

  implicit def DoubleDecodeJson: DecodeJson[Double] =
    optionDecoder(x => if(x.isNull) Some(Double.NaN) else x.number map (_.toDouble), "Double")

  implicit def FloatDecodeJson: DecodeJson[Float] =
    optionDecoder(x => if(x.isNull) Some(Float.NaN) else x.number map (_.toFloat), "Float")

  implicit def IntDecodeJson: DecodeJson[Int] =
    optionDecoder(_.string flatMap (s => try { Some(s.toInt) } catch { case _ => None }), "Int")

  implicit def LongDecodeJson: DecodeJson[Long] =
    optionDecoder(_.string flatMap (s => try { Some(s.toLong) } catch { case _ => None }), "Long")

  implicit def BooleanDecodeJson: DecodeJson[Boolean] =
    optionDecoder(_.bool, "Boolean")

  implicit def CharDecodeJson: DecodeJson[Char] =
    optionDecoder(_.string flatMap (s => if(s.length == 1) Some(s(0)) else None), "Char")

  implicit def JDoubleDecodeJson: DecodeJson[java.lang.Double] =
    optionDecoder(_.number map (_.toDouble), "java.lang.Double")

  implicit def JFloatDecodeJson: DecodeJson[java.lang.Float] =
    optionDecoder(_.number map (_.toFloat), "java.lang.Float")

  implicit def JIntegerDecodeJson: DecodeJson[java.lang.Integer] =
    optionDecoder(_.string flatMap (s => try { Some(s.toInt) } catch { case _ => None }), "java.lang.Integer")

  implicit def JLongDecodeJson: DecodeJson[java.lang.Long] =
    optionDecoder(_.string flatMap (s => try { Some(s.toLong) } catch { case _ => None }), "java.lang.Long")

  implicit def JBooleanDecodeJson: DecodeJson[java.lang.Boolean] =
    optionDecoder(_.bool map (q => q), "java.lang.Boolean")

  implicit def JCharacterDecodeJson: DecodeJson[java.lang.Character] =
    optionDecoder(_.string flatMap (s => if(s == 1) Some(s(0)) else None), "java.lang.Character")

  implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] =
    DecodeJson(a => if(a.focus.isNull)
      DecodeResult(None)
    else
      e(a) map (Some(_))
    )

  def EitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Either[A, B]] =
    DecodeJson(a => {
      val l = (a --\ "Left").success
      val r = (a --\ "Right").success
      (l, r) match {
        case (Some(c), None) => ea(c) map (Left(_))
        case (None, Some(c)) => eb(c) map (Right(_))
        case _ => DecodeResult.failedResult("[A, B]Either[A, B]", a.history)
      }
    })

  def ValidationDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Validation[A, B]] =
    DecodeJson(a => {
      val l = (a --\ "Failure").success
      val r = (a --\ "Success").success
      (l, r) match {
        case (Some(c), None) => ea(c) map (Failure(_))
        case (None, Some(c)) => eb(c) map (Success(_))
        case _ => DecodeResult.failedResult("[A, B]Validation[A, B]", a.history)
      }
    })

  implicit def MapDecodeJson[V](implicit e: DecodeJson[V]): DecodeJson[Map[String, V]] =
    DecodeJson(a =>
      a.fields match {
        case None => DecodeResult.failedResult("[V]Map[String, V]", a.history)
        case Some(s) =>
          s.foldLeftM[DecodeResult, Map[String, V]](Map.empty[String, V])((m, f) =>
            e((a --\ f).hcursor).map(v => m + ((f, v)))
          )
      }
    )

  implicit def Tuple2DecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[(A, B)] =
    DecodeJson(a => a.downArray.hcursor.traverseA[DecodeResult[List[HCursor]]](
      Kleisli[({type λ[+α] = State[DecodeResult[List[HCursor]], α]})#λ, HCursor, ACursor](c =>
        State((x: DecodeResult[List[HCursor]]) =>
          (x map (c :: _), c.right))
      )) exec DecodeResult(Nil) flatMap {
        case cb :: ca :: Nil => for {
          xa <- ea(ca)
          xb <- eb(cb)
        } yield (xa, xb)
        case _ => {
          DecodeResult.failedResult("[A, B]Tuple2[A, B]", a.history)
        }
      })

  implicit def Tuple3DecodeJson[A, B, C](implicit ea: DecodeJson[A], eb: DecodeJson[B], ec: DecodeJson[C]): DecodeJson[(A, B, C)] =
    DecodeJson(a => a.traverseA[DecodeResult[List[HCursor]]](
      Kleisli[({type λ[+α] = State[DecodeResult[List[HCursor]], α]})#λ, HCursor, ACursor](c => {
        State((x: DecodeResult[List[HCursor]]) => {
          (x map (c :: _), c.right)
        })
      })) exec DecodeResult(Nil) flatMap {
        case cc :: cb :: ca :: Nil => for {
          xa <- ea(ca)
          xb <- eb(cb)
          xc <- ec(cc)
        } yield (xa, xb, xc)
        case _ => DecodeResult.failedResult("[A, B, C]Tuple3[A, B, C]", a.history)
      }
    )

  implicit def Tuple4DecodeJson[A, B, C, D](implicit ea: DecodeJson[A], eb: DecodeJson[B], ec: DecodeJson[C], ed: DecodeJson[D]): DecodeJson[(A, B, C, D)] =
    DecodeJson(a => a.traverseA[DecodeResult[List[HCursor]]](
      Kleisli[({type λ[+α] = State[DecodeResult[List[HCursor]], α]})#λ, HCursor, ACursor](c => {
        State((x: DecodeResult[List[HCursor]]) => {
          (x map (c :: _), c.right)
        })
      })) exec DecodeResult(Nil) flatMap {
        case cd :: cc :: cb :: ca :: Nil => for {
          xa <- ea(ca)
          xb <- eb(cb)
          xc <- ec(cc)
          xd <- ed(cd)
        } yield (xa, xb, xc, xd)
        case _ => DecodeResult.failedResult("[A, B, C, D]Tuple4[A, B, C, D]", a.history)
      }
    )

  def jdecode2[A: DecodeJson, B: DecodeJson, X](f: (A, B) => X): DecodeJson[X] =
    implicitly[DecodeJson[(A, B)]].map(x => f(x._1, x._2))

  def jdecode3[A: DecodeJson, B: DecodeJson, C: DecodeJson, X](f: (A, B, C) => X): DecodeJson[X] =
    implicitly[DecodeJson[(A, B, C)]].map(x => f(x._1, x._2, x._3))

  def jdecode4[A: DecodeJson, B: DecodeJson, C: DecodeJson, D: DecodeJson, X](f: (A, B, C, D) => X): DecodeJson[X] =
    implicitly[DecodeJson[(A, B, C, D)]].map(x => f(x._1, x._2, x._3, x._4))

  def jdecode2L[A: DecodeJson, B: DecodeJson, X](f: (A, B) => X)(an: JsonString, bn: JsonString): DecodeJson[X] =
    DecodeJson(x =>
      if(x.focus.obj exists (_.size == 2))
        for {
          aa <- (x --\ an).hcursor.jdecode[A]
          bb <- (x --\ bn).hcursor.jdecode[B]
        } yield f(aa, bb)
      else
        DecodeResult.failedResult("[A, B]Map[String, A|B]", x.history)
    )

  def jdecode3L[A: DecodeJson, B: DecodeJson, C: DecodeJson, X](f: (A, B, C) => X)(an: JsonString, bn: JsonString, cn: JsonString): DecodeJson[X] =
    DecodeJson(x =>
      if(x.focus.obj exists (_.size == 3))
        for {
          aa <- (x --\ an).hcursor.jdecode[A]
          bb <- (x --\ bn).hcursor.jdecode[B]
          cc <- (x --\ cn).hcursor.jdecode[C]
        } yield f(aa, bb, cc)
      else
        DecodeResult.failedResult("[A, B, C]Map[String, A|B|C]", x.history)
    )

  def jdecode4L[A: DecodeJson, B: DecodeJson, C: DecodeJson, D: DecodeJson, X](f: (A, B, C, D) => X)(an: JsonString, bn: JsonString, cn: JsonString, dn: JsonString): DecodeJson[X] =
    DecodeJson(x =>
      if(x.focus.obj exists (_.size == 4))
        for {
          aa <- (x --\ an).hcursor.jdecode[A]
          bb <- (x --\ bn).hcursor.jdecode[B]
          cc <- (x --\ cn).hcursor.jdecode[C]
          dd <- (x --\ dn).hcursor.jdecode[D]
        } yield f(aa, bb, cc, dd)
      else
        DecodeResult.failedResult("[A, B, C, D]Map[String, A|B|C|D]", x.history)
    )
}
