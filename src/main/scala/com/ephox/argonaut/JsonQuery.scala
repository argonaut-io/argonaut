package com.ephox.argonaut

import scalaz._, Scalaz._
import Json._
import JsonValue._

trait JsonQuery {
  val json: Json

  def as[A](implicit from: FromJson[A]) =
    from(json)

  def option[A](path: String*)(implicit from: FromJson[A]) = for {
    j <- find(json, path.toList).map[Option[Json]](v => Some(v)).flatMapError(_ => jsonValue(None))
    r <- j.traverse(x => from.apply(x))
  } yield r

  def value[A](path: String*)(implicit from: FromJson[A]) = for {
    j <- find(json, path.toList)
    r <- j.to[A]
  } yield r

  def list[A](path: String*)(implicit from: FromJson[A]): JsonValue[List[A]] = for {
    j <- find(json, path.toList)
    r <- j.to[List[A]]
  } yield r

  def find(json: Json, path: List[String]): JsonValue[Json] =
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
