package argonaut

import argonaut.JsonMonocle._
import argonaut.JsonObjectMonocle._
import monocle.function.{At, FilterIndex, Index}
import monocle.std.list._
import monocle.{Optional, Traversal}

import scala.language.dynamics

final case class JsonPath(json: Optional[Json, Json]) extends Dynamic {
  def `null`: Optional[Json, Unit] = json composePrism jNullPrism
  def boolean: Optional[Json, Boolean] = json composePrism jBoolPrism
  def byte: Optional[Json, Byte] = json composePrism jBytePrism
  def short: Optional[Json, Short] = json composePrism jShortPrism
  def int: Optional[Json, Int] = json composePrism jIntPrism
  def long: Optional[Json, Long] = json composePrism jLongPrism
  def bigInt: Optional[Json, BigInt] = json composePrism jBigIntPrism
  def bigDecimal: Optional[Json, BigDecimal] = json composePrism jBigDecimalPrism
  def number: Optional[Json, JsonNumber] = json composePrism jNumberPrism
  def string: Optional[Json, String] = json composePrism jStringPrism
  def arr: Optional[Json, List[Json]] = json composePrism jArrayPrism
  def obj: Optional[Json, JsonObject] = json composePrism jObjectPrism

  def at(field: String): Optional[Json, Option[Json]] =
    json.composePrism(jObjectPrism).composeLens(At.at(field))

  def selectDynamic(field: String): JsonPath =
    JsonPath(json.composePrism(jObjectPrism).composeOptional(Index.index(field)))

  def index(i: Int): JsonPath =
    JsonPath(json.composePrism(jArrayPrism).composeOptional(Index.index(i)))

  def each: JsonTraversalPath =
    JsonTraversalPath(json composeTraversal jDescendants)

  def arrFilter(p: Int => Boolean): JsonTraversalPath =
    JsonTraversalPath(arr composeTraversal FilterIndex.filterIndex(p))

  def objFilter(p: String => Boolean): JsonTraversalPath =
    JsonTraversalPath(obj composeTraversal FilterIndex.filterIndex(p))
}

object JsonPath {
  val root: JsonPath = JsonPath(Optional.id)
}

final case class JsonTraversalPath(json: Traversal[Json, Json]) extends Dynamic {
  def `null`: Traversal[Json, Unit] = json composePrism jNullPrism
  def boolean: Traversal[Json, Boolean] = json composePrism jBoolPrism
  def byte: Traversal[Json, Byte] = json composePrism jBytePrism
  def short: Traversal[Json, Short] = json composePrism jShortPrism
  def int: Traversal[Json, Int] = json composePrism jIntPrism
  def long: Traversal[Json, Long] = json composePrism jLongPrism
  def bigInt: Traversal[Json, BigInt] = json composePrism jBigIntPrism
  def bigDecimal: Traversal[Json, BigDecimal] = json composePrism jBigDecimalPrism
  def number: Traversal[Json, JsonNumber] = json composePrism jNumberPrism
  def string: Traversal[Json, String] = json composePrism jStringPrism
  def arr: Traversal[Json, List[Json]] = json composePrism jArrayPrism
  def obj: Traversal[Json, JsonObject] = json composePrism jObjectPrism

  def at(field: String): Traversal[Json, Option[Json]] =
    json.composePrism(jObjectPrism).composeLens(At.at(field))

  def selectDynamic(field: String): JsonTraversalPath =
    JsonTraversalPath(json.composePrism(jObjectPrism).composeOptional(Index.index(field)))

  def index(i: Int): JsonTraversalPath =
    JsonTraversalPath(json.composePrism(jArrayPrism).composeOptional(Index.index(i)))

  def each: JsonTraversalPath =
    JsonTraversalPath(json composeTraversal jDescendants)

  def arrFilter(p: Int => Boolean): JsonTraversalPath =
    JsonTraversalPath(arr composeTraversal FilterIndex.filterIndex(p))

  def objFilter(p: String => Boolean): JsonTraversalPath =
    JsonTraversalPath(obj composeTraversal FilterIndex.filterIndex(p))
}