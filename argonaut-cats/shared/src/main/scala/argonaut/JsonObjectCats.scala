package argonaut

import Json._
import JsonObject._
import cats._
import syntax.foldable._, syntax.functor._

object JsonObjectCats extends JsonObjectCatss {
  def from[F[_]: Foldable](f: F[(JsonField, Json)]): JsonObject = {
    f.foldLeft(empty) { case (acc, (k, v)) => acc + (k, v) }
  }
}

trait JsonObjectCatss {
  implicit val JsonObjectEq: Eq[JsonObject] = Eq.fromUniversalEquals[JsonObject]

  implicit val JsonObjectShow: Show[JsonObject] = Show.fromToString[JsonObject]

  def traverse[F[_]](o: JsonObject, f: Json => F[Json])(implicit FF: Applicative[F]): F[JsonObject] = {
    o.toList.foldLeft(FF.pure(List[JsonAssoc]())){case (acc, (k, v)) =>
      FF.map2(acc, f(v)){(elems, newV) =>
        (k, newV) :: elems
      }
    }.map(elems => JsonObject.fromIterable(elems.reverse))
  }

}
