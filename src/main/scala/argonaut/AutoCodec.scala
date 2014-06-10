package argonaut

import argonaut._, Argonaut._
import scalaz.{ Coproduct => _, _}, Scalaz._
import shapeless._

object AutoEncodeJson extends LabelledTypeClassCompanion[EncodeJson] {

  implicit def EncodeJsonTypeClass: LabelledTypeClass[EncodeJson] = new LabelledTypeClass[EncodeJson] {
    // what would this look like...
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
}

object AutoDecodeJson extends LabelledTypeClassCompanion[DecodeJson] {

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
}
