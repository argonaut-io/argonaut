package argonaut

import Argonaut._
import cats._, data._

object EncodeJsonCats extends EncodeJsonCatss {
}

trait EncodeJsonCatss {
  def fromFoldable[F[_], A](implicit A: EncodeJson[A], F: Foldable[F]): EncodeJson[F[A]] =
    EncodeJson(fa => jArray(F.foldLeft(fa, Nil: List[Json])((list, a) => A.encode(a) :: list).reverse))

  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }

  implicit val EncodeJsonNumberInstance: Contravariant[EncodeJsonNumber] = new Contravariant[EncodeJsonNumber] {
    def contramap[A, B](r: EncodeJsonNumber[A])(f: B => A) = r contramap f
  }

  implicit val EncodePossibleJsonNumberInstance: Contravariant[EncodePossibleJsonNumber] = new Contravariant[EncodePossibleJsonNumber] {
    def contramap[A, B](r: EncodePossibleJsonNumber[A])(f: B => A) = r contramap f
  }

  implicit def NonEmptyListEncodeJson[A: EncodeJson]: EncodeJson[NonEmptyList[A]] =
    fromFoldable[NonEmptyList, A]

  implicit def ValidatedEncodeJson[E, A](implicit EA: EncodeJson[E], EB: EncodeJson[A]): EncodeJson[Validated[E, A]] =
    EncodeJson(_.fold (
      e => jSingleObject("Invalid", EA(e)), a => jSingleObject("Valid", EB(a))
    ))
}
