package argonaut

import Json._
import scalaz._
import scalaz.syntax.foldable._
import scalaz.syntax.functor._
import JsonObject._, Json._

object JsonObjectScalaz extends JsonObjectScalazs {
  def from[F[_]: Foldable](f: F[(JsonField, Json)]): JsonObject = {
    f.foldLeft(empty){ case (acc, (k, v)) => acc + (k, v) }
  }
}

trait JsonObjectScalazs {
  /**
   * The lens to the JSON value.
   */
  def jsonObjectL(f: JsonField): JsonObject @> Option[Json] = {
    Lens(jsonObject => Store(_ match {
      case None => jsonObject - f
      case Some(v) => jsonObject + (f, v)
    }, jsonObject(f)))
  }

  /**
   * The partial lens to the JSON value.
   */
  def jsonObjectPL(f: JsonField): JsonObject @?> Json = {
    PLens.somePLens compose ~jsonObjectL(f)
  }

  implicit val JsonObjectShow: Show[JsonObject] = Show.showFromToString[JsonObject]

  implicit val JsonObjectEqual: Equal[JsonObject] = Equal.equalA[JsonObject]

  def traverse[F[_]](o: JsonObject, f: Json => F[Json])(implicit FF: Applicative[F]): F[JsonObject] = {
    o.toList.foldLeft(FF.point(List[JsonAssoc]())){case (acc, (k, v)) =>
      FF.apply2(acc, f(v)){(elems, newV) =>
        (k, newV) :: elems
      }
    }.map(elems => JsonObject.fromIterable(elems.reverse))
  }
}
