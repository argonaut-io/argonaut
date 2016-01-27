package argonaut

import Json._
import JsonObject._
import cats._
import syntax.foldable._

object JsonObjectCats extends JsonObjectCatss {
  def from[F[_]: Foldable](f: F[(JsonField, Json)]): JsonObject = {
    f.foldLeft(empty) { case (acc, (k, v)) => acc + (k, v) }
  }
}

trait JsonObjectCatss {
  implicit val JsonObjectEq = Eq.fromUniversalEquals[JsonObject]

  implicit val JsonObjectShow = Show.fromToString[JsonObject]
}
