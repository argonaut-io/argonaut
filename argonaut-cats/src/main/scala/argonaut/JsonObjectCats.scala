package argonaut

import argonaut.Json._
import argonaut.JsonObject._
import cats._
import cats.syntax.foldable._

object JsonObjectCats extends JsonObjectCatss {
  def from[F[_]: Foldable](f: F[(JsonField, Json)]): JsonObject = {
    f.foldLeft(empty) { case (acc, (k, v)) => acc + (k, v) }
  }
}

trait JsonObjectCatss {
  implicit val JsonObjectShow = Show.fromToString[JsonObject]

  implicit val JsonObjectEqual = Eq.fromUniversalEquals[JsonObject]
}
