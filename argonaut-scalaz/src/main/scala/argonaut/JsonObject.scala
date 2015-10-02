package argonaut

import Json._
import scalaz._, syntax.traverse._, syntax.show._
import std.tuple._, std.string._

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

  implicit val JsonObjectShow = Show.showFromToString[JsonObject]

  implicit val JsonObjectEqual = Equal.equalA[JsonObject]
}
