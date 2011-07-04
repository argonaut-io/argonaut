package com.ephox.argonaut

import scalaz._, Scalaz._
import Json._
import JsonValue._
import JsonQuery._

trait JsonQuery {
  val json: Json

  def as[A](implicit from: FromJson[A]) =
    from(json)

  def value[A](path: String*)(implicit from: FromJson[A]) = for {
    j <- findjson(json, path.toList)
    r <- j.as[A]
  } yield r

  def option[A](path: String*)(implicit from: FromJson[A]) = for {
    j <- findjson(json, path.toList).map[Option[Json]](v => Some(v)).flatMapError(_ => jsonValue(None))
    r <- j.traverse(x => from.apply(x))
  } yield r

  def findjson(json: Json, path: List[String]): JsonValue[Json] =
    (json -|| path).json(
      j => jsonValue(j),
      error(json, path, "does not exist")
    )

  def error[A](json: Json, path: List[String], note: String): JsonValue[A] =
    jsonError[A]("Path [" + path.mkString("/") + "] " + note + ", in json [\n" + JsonPrinter.pretty(json)+ "\n]")
}

object JsonQuery extends JsonQuerys

trait JsonQuerys {
  implicit def JsonToJsonQuery(j: Json): JsonQuery = new JsonQuery {
    val json = j
  }
}
