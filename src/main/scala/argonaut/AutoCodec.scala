package argonaut

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import shapeless._

object AutoEncodeJson extends LabelledProductTypeClassCompanion[EncodeJson] {
  implicit def EncodeJsonTypeClass: LabelledProductTypeClass[EncodeJson] = new LabelledProductTypeClass[EncodeJson] {
    def emptyProduct =
      EncodeJson(_ => jEmptyObject)

    def product[A, T <: HList](name: String, A: EncodeJson[A], T: EncodeJson[T]) =
      EncodeJson(a => (name -> A.encode(a.head)) ->: T.encode(a.tail))

    def project[F, G](instance: => EncodeJson[G], to : F => G, from : G => F) =
      instance.contramap(to)
  }
}

object AutoDecodeJson extends LabelledProductTypeClassCompanion[DecodeJson] {
  implicit def DecodeJsonTypeClass: LabelledProductTypeClass[DecodeJson] = new LabelledProductTypeClass[DecodeJson] {
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
