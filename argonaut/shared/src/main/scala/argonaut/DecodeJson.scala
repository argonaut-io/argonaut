package argonaut

import scala.collection.mutable.Builder
import scala.util.control.Exception.catching
import Json._

trait DecodeJson[A] {
  /**
   * Decode the given hcursor. Alias for `decode`.
   */
  def apply(c: HCursor): DecodeResult[A] = decode(c)

  /**
   * Decode the given hcursor.
   */
  def decode(c: HCursor): DecodeResult[A]

  /**
   * Decode the given acursor.
   */
  def tryDecode(c: ACursor): DecodeResult[A] = c.either match {
    case Left(invalid) => DecodeResult.fail("Attempt to decode value on failed cursor.", invalid.history)
    case Right(valid) => decode(valid)
  }

  /**
   * Decode the given json.
   */
  def decodeJson(j: Json): DecodeResult[A] = decode(j.hcursor)

  /**
    * Transform the incoming HCursor to produce another DecodeJson instance.
    */
  def flatMapCursor(f: HCursor => DecodeResult[HCursor]): DecodeJson[A] = {
    val original = this
    (c: HCursor) => for {
      fr <- f(c)
      result <- original.decode(fr)
    } yield result
  }

  /**
   * Covariant functor.
   */
  def map[B](f: A => B): DecodeJson[B] = {
    def thisDecode = decode(_)
    def thisTryDecode = tryDecode(_)
    new DecodeJson[B] {
      override def decode(c: HCursor): DecodeResult[B] = {
        thisDecode(c).map(f)
      }

      override def tryDecode(c: ACursor): DecodeResult[B] = {
        thisTryDecode(c).map(f)
      }
    }
  }

  /**
   * Monad.
   */
  def flatMap[B](f: A => DecodeJson[B]): DecodeJson[B] = {
    def thisDecode = decode(_)
    def thisTryDecode = tryDecode(_)
    new DecodeJson[B] {
      override def decode(c: HCursor): DecodeResult[B] = {
        thisDecode(c).flatMap(a => f(a).decode(c))
      }

      override def tryDecode(c: ACursor): DecodeResult[B] = {
        thisTryDecode(c).flatMap(a => f(a).tryDecode(c))
      }
    }
  }

  /**
   * Build a new DecodeJson codec with the specified name.
   */
  def setName(n: String): DecodeJson[A] = {
    DecodeJson[A](c => apply(c).result.fold(
      { case (_, h) => DecodeResult.fail(n, h) },
      a => DecodeResult.ok(a)
    ))
  }

  /**
   * Build a new DecodeJson codec with the specified precondition that f(c) == true.
   */
  def validate(f: HCursor => Boolean, message: => String): DecodeJson[A] = {
    DecodeJson(c => if (f(c)) apply(c) else DecodeResult.fail[A](message, c.history))
  }

  /**
   * Build a new DecodeJson codec with the precondition that the cursor focus is object with exactly n field.
   */
  def validateFields(n: Int): DecodeJson[A] = {
    validate(_.focus.obj exists (_.size == n), "Expected json object with exactly [" + n + "] fields.")
  }

  /**
   * Combine two decoders.
   */
  def &&&[B](x: DecodeJson[B]): DecodeJson[(A, B)] = {
    DecodeJson(j => for {
      a <- this(j)
      b <- x(j)
    } yield (a, b))
  }

  /**
   * Choose the first succeeding decoder.
   */
  def |||[B, AA](x: => DecodeJson[B])(implicit evA: A <:< AA, evB: B <:< AA): DecodeJson[AA] = {
    DecodeJson[AA](c => {
      val q = apply(c).map(evA)
      q.result.fold(_ => x(c).map(evB), _ => q)
    })
  }

  /**
   * Run one or another decoder.
   */
  def split[B](x: DecodeJson[B]): Either[HCursor, HCursor] => DecodeResult[Either[A, B]] = {
    c => c.fold(a => this(a).map(Left.apply), a => x(a).map(Right.apply))
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

  /**
   * Widen A into AA.
   */
  def widen[AA]()(implicit ev: A <:< AA): DecodeJson[AA] = map(ev)

}

object DecodeJson extends DecodeJsons with DecodeJsonMacro {
  def apply[A](r: HCursor => DecodeResult[A]): DecodeJson[A] = {
    (c: HCursor) => r(c)
  }

  def withReattempt[A](r: ACursor => DecodeResult[A]): DecodeJson[A] = {
    new DecodeJson[A] {
      def decode(c: HCursor): DecodeResult[A] = tryDecode(c.acursor)

      override def tryDecode(c: ACursor) = r(c)
    }
  }

  def of[A: DecodeJson]: DecodeJson[A] = implicitly[DecodeJson[A]]
}

trait DecodeJsons extends GeneratedDecodeJsons {

  def optionDecoder[A](k: Json => Option[A], e: String): DecodeJson[A] = {
    DecodeJson[A](a => k(a.focus) match {
      case None => DecodeResult.fail(e, a.history)
      case Some(w) => DecodeResult.ok(w)
    })
  }

  /**
   * Construct a succeeding decoder from the given function.
   */
  def decodeArr[A](f: HCursor => A): DecodeJson[A] = DecodeJson(j => DecodeResult.ok(f(j)))

  def tryTo[A](f: => A): Option[A] = catching(classOf[IllegalArgumentException]).opt(f)

  implicit def HCursorDecodeJson: DecodeJson[HCursor] = decodeArr(q => q)

  implicit def JsonDecodeJson: DecodeJson[Json] = decodeArr(j => j.focus)

  def BuilderDecodeJson[A, C[_]](newBuilder: () => Builder[A, C[A]])(implicit e: DecodeJson[A]): DecodeJson[C[A]] = {
    DecodeJson(a =>
      a.downArray.hcursor match {
        case None => {
          if (a.focus.isArray) {
            DecodeResult.ok(newBuilder.apply().result())
          } else {
            DecodeResult.fail("[A]", a.history)
          }
        }
        case Some(hcursor) => {
          hcursor.rights match {
            case Some(elements) => {
              (hcursor.focus :: elements).foldLeft(DecodeResult.ok(newBuilder.apply())){(working, elem) =>
                for {
                  w <- working
                  e <- elem.jdecode[A]
                } yield w += e
              }.map(_.result())
            }
            case _ => {
              DecodeResult.fail("[A]", a.history)
            }
          }
        }
      }
    )
  }

  implicit val UUIDDecodeJson: DecodeJson[java.util.UUID] = {
    optionDecoder(_.string.flatMap(s =>
      tryTo(java.util.UUID.fromString(s))), "UUID")
  }

  implicit def ListDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[List[A]] =
    BuilderDecodeJson[A, List](() => List.newBuilder)

  implicit def VectorDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Vector[A]] =
    BuilderDecodeJson[A, Vector](() => Vector.newBuilder)

  implicit def StreamDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Stream[A]] =
    BuilderDecodeJson[A, Stream](() => Stream.newBuilder)

  implicit def SetDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Set[A]] =
    BuilderDecodeJson[A, Set](() => Set.newBuilder)

  implicit def UnitDecodeJson: DecodeJson[Unit] = {
    DecodeJson.apply[Unit](a => if (a.focus.isNull || a.focus == jEmptyObject || a.focus == jEmptyArray) {
      DecodeResult.ok(())
    } else {
      DecodeResult.fail("Unit", a.history)
    })
  }

  implicit def StringDecodeJson: DecodeJson[String] = optionDecoder(_.string, "String")

  implicit def DoubleDecodeJson: DecodeJson[Double] = {
    optionDecoder[Double](x => {
      if (x.isNull) {
        Some(Double.NaN)
      } else {
        x.number.flatMap(n => tryTo(n.truncateToDouble)).orElse(x.string.flatMap(s => tryTo(s.toDouble)))
      }
    }, "Double")
  }

  implicit def FloatDecodeJson: DecodeJson[Float] = {
    optionDecoder[Float](x => {
      if (x.isNull) {
        Some(Float.NaN)
      } else {
        x.number.flatMap(n => tryTo(n.truncateToFloat)).orElse(x.string.flatMap(s => tryTo(s.toFloat)))
      }
    }, "Float")
  }

  implicit def IntDecodeJson: DecodeJson[Int] = {
    optionDecoder(x =>
      (x.number map (_.truncateToInt)).orElse(
      (x.string flatMap (s => tryTo(s.toInt)))), "Int")
  }

  implicit def LongDecodeJson: DecodeJson[Long] = {
    optionDecoder(x =>
      (x.number map (_.truncateToLong)).orElse(
      (x.string flatMap (s => tryTo(s.toLong)))), "Long")
  }

  implicit def ShortDecodeJson: DecodeJson[Short] = {
    optionDecoder(x =>
      (x.number map (_.truncateToShort)).orElse(
      (x.string flatMap (s => tryTo(s.toShort)))), "Short")
  }

  implicit def ByteDecodeJson: DecodeJson[Byte] = {
    optionDecoder(x =>
      (x.number map (_.truncateToByte)).orElse(
      (x.string flatMap (s => tryTo(s.toByte)))), "Byte")
  }

  implicit def BigIntDecodeJson: DecodeJson[BigInt] = {
    optionDecoder(x =>
      (x.number flatMap (_.truncateToBigInt)).orElse(
      (x.string flatMap (s => tryTo(BigInt(s))))), "BigInt")
  }

  implicit def BigDecimalDecodeJson: DecodeJson[BigDecimal] = {
    optionDecoder(x =>
      (x.number map (_.toBigDecimal)).orElse(
      (x.string flatMap (s => tryTo(BigDecimal(s))))), "BigDecimal")
  }

  implicit def BooleanDecodeJson: DecodeJson[Boolean] = {
    optionDecoder(x => x.bool.orElse(x.string.flatMap(s => tryTo(s.toBoolean))), "Boolean")
  }

  implicit def CharDecodeJson: DecodeJson[Char] = {
    optionDecoder(_.string.flatMap(s => if(s.length == 1) Some(s(0)) else None), "Char")
  }

  implicit def JDoubleDecodeJson: DecodeJson[java.lang.Double] = {
    DoubleDecodeJson.map(d => d: java.lang.Double)
  }

  implicit def JFloatDecodeJson: DecodeJson[java.lang.Float] = {
    FloatDecodeJson.map(f => f: java.lang.Float)
  }

  implicit def JIntegerDecodeJson: DecodeJson[java.lang.Integer] = {
    optionDecoder(_.number.flatMap(s => tryTo(s.truncateToInt)), "java.lang.Integer")
  }

  implicit def JLongDecodeJson: DecodeJson[java.lang.Long] = {
    optionDecoder(_.number.flatMap(s => tryTo(s.truncateToLong)), "java.lang.Long")
  }

  implicit def JShortDecodeJson: DecodeJson[java.lang.Short] = {
    optionDecoder(_.number.flatMap(s => tryTo(s.truncateToShort)), "java.lang.Short")
  }

  implicit def JByteDecodeJson: DecodeJson[java.lang.Byte] = {
    optionDecoder(_.number.flatMap(s => tryTo(s.truncateToByte)), "java.lang.Byte")
  }

  implicit def JBooleanDecodeJson: DecodeJson[java.lang.Boolean] = {
    optionDecoder(_.bool.map(q => q), "java.lang.Boolean")
  }

  implicit def JCharacterDecodeJson: DecodeJson[java.lang.Character] = {
    optionDecoder(_.string.flatMap(s => if(s.length == 1) Some(s(0)) else None), "java.lang.Character")
  }

  implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] = {
    DecodeJson.withReattempt[Option[A]](a => a.success match {
      case None => DecodeResult.ok(None)
      case Some(valid) => {
        if (valid.focus.isNull) {
          DecodeResult.ok(None)
        } else {
          e(valid).option
        }
      }
    })
  }

  implicit def EitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Either[A, B]] = {
    DecodeJson[Either[A, B]](a => {
      val l = (a --\ "Left").success
      val r = (a --\ "Right").success
      (l, r) match {
        case (Some(c), None) => ea(c) map (Left(_))
        case (None, Some(c)) => eb(c) map (Right(_))
        case _ => DecodeResult.fail("[A, B]Either[A, B]", a.history)
      }
    })
  }

  implicit def MapDecodeJson[K, V](implicit dk: DecodeJson[K], dv: DecodeJson[V]): DecodeJson[Map[K, V]] = {
    DecodeJson(a =>
      a.fields match {
        case None => DecodeResult.fail("[K, V]Map[K, V]", a.history)
        case Some(s) => {
          def spin(x: List[JsonField], m: DecodeResult[Map[K, V]]): DecodeResult[Map[K, V]] = {
            x match {
              case Nil => m
              case h::t => {
                spin(t, for {
                  mm <- m
                  k <- dk.decodeJson(jString(h))
                  v <- a.get(h)(dv)
                } yield mm + ((k, v)))
              }
            }
          }
          spin(s, DecodeResult.ok(Map.empty[K, V]))
        }
      }
    )
  }
}
