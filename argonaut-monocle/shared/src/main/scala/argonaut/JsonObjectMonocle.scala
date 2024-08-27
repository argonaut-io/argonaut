package argonaut

import argonaut.Json.*
import monocle.function.At
import monocle.function.Each
import monocle.function.FilterIndex
import monocle.function.Index
import monocle.Lens
import monocle.Traversal
import cats.Applicative
import cats.syntax.all.*

object JsonObjectMonocle extends JsonObjectMonocles

trait JsonObjectMonocles {
  implicit val jObjectEach: Each[JsonObject, Json] = new Each[JsonObject, Json] {
    def each: Traversal[JsonObject, Json] = new Traversal[JsonObject, Json] {
      def modifyA[F[_]: Applicative](f: Json => F[Json])(from: JsonObject): F[JsonObject] = {
        JsonObjectCats.traverse(from, f)
      }
    }
  }

  implicit val jObjectAt: At[JsonObject, JsonField, Option[Json]] = new At[JsonObject, JsonField, Option[Json]] {
    def at(field: JsonField): Lens[JsonObject, Option[Json]] =
      monocle.Lens[JsonObject, Option[Json]](_.apply(field))(optVal =>
        jObj => optVal.fold(jObj - field)(value => jObj + (field, value))
      )
  }

  implicit val jObjectFilterIndex: FilterIndex[JsonObject, JsonField, Json] =
    new FilterIndex[JsonObject, JsonField, Json] {
      def filterIndex(predicate: JsonField => Boolean): Traversal[JsonObject, Json] = new Traversal[JsonObject, Json] {
        def modifyA[F[_]: Applicative](f: Json => F[Json])(from: JsonObject): F[JsonObject] =
          Applicative[F].map(
            from.toList.traverse[F, (JsonField, Json)] { case (field, json) =>
              Applicative[F].map(if (predicate(field)) f(json) else json.pure[F])(field -> _)
            }
          )(JsonObject.fromIterable(_))
      }
    }

  implicit val jObjectIndex: Index[JsonObject, JsonField, Json] = Index.fromAt
}
