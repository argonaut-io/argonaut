package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import shapeless._

object AutoEncodeJson extends LabelledProductTypeClassCompanion[EncodeJson] {
 implicit def EncodeJsonTypeClass: LabelledProductTypeClass[EncodeJson] = new LabeledProductTypeClass[EncodeJson] {
    def emptyProduct =
      jEmptyObject

    def product[A, T <: HList](name: String, A: Codec[A], T: Codec[T]) =
      ???

    def project[F, G](instance: => Codec[G], to : F => G, from : G => F) =
      ???
 }

}
