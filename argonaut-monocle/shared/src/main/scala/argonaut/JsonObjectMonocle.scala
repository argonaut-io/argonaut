package argonaut

import argonaut.Json._
import monocle.function.{At, Each, FilterIndex, Index}
import monocle.{Lens, Traversal}

import scalaz.Applicative
import scalaz.std.list._
import scalaz.syntax.applicative._

object JsonObjectMonocle extends JsonObjectMonocles

trait JsonObjectMonocles {
  implicit val jObjectEach: Each[JsonObject, Json] = new Each[JsonObject, Json]{
    def each = new Traversal[JsonObject, Json]{
      def modifyF[F[_]: Applicative](f: Json => F[Json])(from: JsonObject): F[JsonObject] = {
        JsonObjectScalaz.traverse(from, f)
      }
    }
  }

  implicit val jObjectAt: At[JsonObject, JsonField, Option[Json]] = new At[JsonObject, JsonField, Option[Json]]{
    def at(field: JsonField): Lens[JsonObject, Option[Json]] =
      monocle.Lens[JsonObject, Option[Json]](_.apply(field))( optVal => jObj =>
        optVal.fold(jObj - field)(value => jObj + (field, value))
      )
  }

  implicit val jObjectFilterIndex: FilterIndex[JsonObject, JsonField, Json] = new FilterIndex[JsonObject, JsonField, Json]{
    import scalaz.syntax.traverse._
    def filterIndex(predicate: JsonField => Boolean) = new Traversal[JsonObject, Json]{
      def modifyF[F[_]: Applicative](f: Json => F[Json])(from: JsonObject): F[JsonObject] =
        Applicative[F].map(
          from.toList.traverse[F, (JsonField, Json)]{ case (field, json) =>
            Applicative[F].map(if(predicate(field)) f(json) else json.point[F])(field -> _)
          }
        )(JsonObject.fromIterable(_))
    }
  }

  implicit val jObjectIndex: Index[JsonObject, JsonField, Json] = Index.fromAt
}
