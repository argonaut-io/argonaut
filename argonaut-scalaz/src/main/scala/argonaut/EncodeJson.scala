package argonaut

import scalaz._, syntax.either._
import Json._

object EncodeJsonScalaz extends EncodeJsonScalazs {
}

trait EncodeJsonScalazs {
  def fromFoldable[F[_], A](implicit A: EncodeJson[A], F: Foldable[F]): EncodeJson[F[A]] =
    EncodeJson(fa => jArray(F.foldLeft(fa, Nil: List[Json])((list, a) => A.encode(a) :: list).reverse))

  implicit def MaybeEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Maybe[A]] = EncodeJson(_.cata(e(_), jNull))

  implicit def DisjunctionEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[A \/ B] =
    EncodeJson(_.fold(
      a => jSingleObject("Left", ea(a)),
      b => jSingleObject("Right", eb(b))
    ))

  implicit def ValidationEncodeJson[E, A](implicit ea: EncodeJson[E], eb: EncodeJson[A]): EncodeJson[Validation[E, A]] =
    EncodeJson(_ fold (
      e => jSingleObject("Failure", ea(e)), a => jSingleObject("Success", eb(a))
    ))

  implicit def IListEncodeJson[A: EncodeJson]: EncodeJson[IList[A]] =
    fromFoldable[IList, A]

  implicit def DListEncodeJson[A: EncodeJson]: EncodeJson[DList[A]] =
    fromFoldable[DList, A]

  implicit def EphemeralStreamEncodeJson[A: EncodeJson]: EncodeJson[EphemeralStream[A]] =
    fromFoldable[EphemeralStream, A]

  implicit def ISetEncodeJson[A: EncodeJson]: EncodeJson[ISet[A]] =
    fromFoldable[ISet, A]

  implicit def NonEmptyListEncodeJson[A: EncodeJson]: EncodeJson[NonEmptyList[A]] =
    fromFoldable[NonEmptyList, A]

  implicit def IMapEncodeJson[A, B](implicit A: EncodeJsonKey[A], B: EncodeJson[B]): EncodeJson[A ==>> B] =
    EncodeJson(x => jObjectAssocList(
      x.foldrWithKey(Nil: List[(String, Json)])(
        (k, v, list) => (A.toJsonKey(k), B(v)) :: list
      )
    ))

  implicit val EncodeJsonContra: Contravariant[EncodeJson] = new Contravariant[EncodeJson] {
    def contramap[A, B](r: EncodeJson[A])(f: B => A) = r contramap f
  }
}
