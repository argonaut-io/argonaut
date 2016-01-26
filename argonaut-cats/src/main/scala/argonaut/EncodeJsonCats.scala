package argonaut

import Argonaut._
import cats._, data._, std.all._
import cats.functor.Contravariant

object EncodeJsonCats extends EncodeJsonCatss {
}

trait EncodeJsonCatss {
  def fromFoldable[F[_], A](implicit A: EncodeJson[A], F: Foldable[F]): EncodeJson[F[A]] =
    EncodeJson(fa => jArray(F.foldLeft(fa, Nil: List[Json])((list, a) => A.encode(a) :: list).reverse))

  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }

  implicit def NonEmptyListEncodeJson[A: EncodeJson]: EncodeJson[NonEmptyList[A]] =
    fromFoldable[NonEmptyList, A]

  implicit def StreamingEncodeJson[A: EncodeJson]: EncodeJson[Streaming[A]] =
    fromFoldable[Streaming, A]

  implicit def ValidatedEncodeJson[E, A](implicit ea: EncodeJson[E], eb: EncodeJson[A]): EncodeJson[Validated[E, A]] =
    EncodeJson(_ fold (
      e => jSingleObject("Invalid", ea(e)), a => jSingleObject("Valid", eb(a))
    ))

  implicit def XorEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Xor[A, B]] =
    EncodeJson(_.fold(
      a => jSingleObject("Left", ea(a)),
      b => jSingleObject("Right", eb(b))
    ))
}
