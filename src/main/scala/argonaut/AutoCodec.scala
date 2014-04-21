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

  implicit def DecodeJsonTypeClass: LabelledProductTypeClass[DecodeJson] = new LabelledProductTypeClass[DecodeJson] {
    def emptyProduct =
      ???

    def product[A, T <: HList](name: String, A: DecodeJson[A], T: DecodeJson[T]) =
      ???

    def project[F, G](instance: => DecodeJson[G], to : F => G, from : G => F) =
      instance.map(from)
  }
}
