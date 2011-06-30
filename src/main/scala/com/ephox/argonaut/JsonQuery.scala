package com.ephox.argonaut

import scalaz._, Scalaz._
import Json._
import JsonValue._

trait JsonQuery {
  def value[A](json: Json, path: String*)(implicit from: FromJson[A]) = for {
    j <- find(json, path.toList)
    r <- j.to[A]
  } yield r

  def list[A](json: Json, path: String*)(implicit from: FromJson[A]): JsonValue[List[A]] = for {
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

