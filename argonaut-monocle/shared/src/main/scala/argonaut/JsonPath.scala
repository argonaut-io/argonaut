package argonaut

import argonaut.JsonMonocle._
import argonaut.JsonObjectMonocle._
import monocle.function.At
import monocle.function.FilterIndex
import monocle.function.Index
import monocle.Fold
import monocle.Optional
import monocle.Traversal
import scala.language.dynamics
import cats.Monoid

final case class JsonPath(json: Optional[Json, Json]) extends Dynamic {
  def `null`: Optional[Json, Unit] = json andThen jNullPrism
  def boolean: Optional[Json, Boolean] = json andThen jBoolPrism
  def byte: Optional[Json, Byte] = json andThen jBytePrism
  def short: Optional[Json, Short] = json andThen jShortPrism
  def int: Optional[Json, Int] = json andThen jIntPrism
  def long: Optional[Json, Long] = json andThen jLongPrism
  def bigInt: Optional[Json, BigInt] = json andThen jBigIntPrism
  def bigDecimal: Optional[Json, BigDecimal] = json andThen jBigDecimalPrism
  def number: Optional[Json, JsonNumber] = json andThen jNumberPrism
  def string: Optional[Json, String] = json andThen jStringPrism
  def arr: Optional[Json, List[Json]] = json andThen jArrayPrism
  def obj: Optional[Json, JsonObject] = json andThen jObjectPrism

  def at(field: String): Optional[Json, Option[Json]] =
    json andThen jObjectPrism andThen At.at(field)

  def selectDynamic(field: String): JsonPath =
    JsonPath(json andThen jObjectPrism andThen Index.index(field))

  def applyDynamic(field: String)(i: Int): JsonPath = selectDynamic(field).index(i)

  def apply(i: Int): JsonPath = index(i)

  def index(i: Int): JsonPath =
    JsonPath(json andThen jArrayPrism andThen Index.index[List[Json], Int, Json](i))

  def each: JsonTraversalPath =
    JsonTraversalPath(json andThen jDescendants)

  def filterByIndex(p: Int => Boolean): JsonTraversalPath =
    JsonTraversalPath(arr andThen FilterIndex.filterIndex[List[Json], Int, Json](p))

  def filterByField(p: String => Boolean): JsonTraversalPath =
    JsonTraversalPath(obj andThen FilterIndex.filterIndex(p))

  final def filter(p: Json => Boolean): JsonFoldPath =
    JsonFoldPath(json andThen OpticsHelper.select(p))

  final def as[A: DecodeJson]: Fold[Json, A] =
    json andThen OpticsHelper.parse
}

object JsonPath extends JsonPaths

trait JsonPaths {
  val root: JsonPath = JsonPath(monocle.Iso.id)
}

final case class JsonTraversalPath(json: Traversal[Json, Json]) extends Dynamic {
  def `null`: Traversal[Json, Unit] = json andThen jNullPrism
  def boolean: Traversal[Json, Boolean] = json andThen jBoolPrism
  def byte: Traversal[Json, Byte] = json andThen jBytePrism
  def short: Traversal[Json, Short] = json andThen jShortPrism
  def int: Traversal[Json, Int] = json andThen jIntPrism
  def long: Traversal[Json, Long] = json andThen jLongPrism
  def bigInt: Traversal[Json, BigInt] = json andThen jBigIntPrism
  def bigDecimal: Traversal[Json, BigDecimal] = json andThen jBigDecimalPrism
  def number: Traversal[Json, JsonNumber] = json andThen jNumberPrism
  def string: Traversal[Json, String] = json andThen jStringPrism
  def arr: Traversal[Json, List[Json]] = json andThen jArrayPrism
  def obj: Traversal[Json, JsonObject] = json andThen jObjectPrism

  def at(field: String): Traversal[Json, Option[Json]] =
    json andThen jObjectPrism andThen At.at(field)

  def selectDynamic(field: String): JsonTraversalPath =
    JsonTraversalPath(json andThen jObjectPrism andThen Index.index(field))

  def applyDynamic(field: String)(i: Int): JsonTraversalPath = selectDynamic(field).index(i)

  def apply(i: Int): JsonTraversalPath = index(i)

  def index(i: Int): JsonTraversalPath =
    JsonTraversalPath(json andThen jArrayPrism andThen Index.index[List[Json], Int, Json](i))

  def each: JsonTraversalPath =
    JsonTraversalPath(json andThen jDescendants)

  def filterByIndex(p: Int => Boolean): JsonTraversalPath =
    JsonTraversalPath(arr andThen FilterIndex.filterIndex[List[Json], Int, Json](p))

  def filterByField(p: String => Boolean): JsonTraversalPath =
    JsonTraversalPath(obj andThen FilterIndex.filterIndex(p))

  final def filter(p: Json => Boolean): JsonFoldPath =
    JsonFoldPath(json andThen OpticsHelper.select(p))

  final def as[A: DecodeJson]: Fold[Json, A] =
    json andThen OpticsHelper.parse
}

final case class JsonFoldPath(json: Fold[Json, Json]) extends Dynamic {
  final def `null`: Fold[Json, Unit] = json andThen jNullPrism
  final def boolean: Fold[Json, Boolean] = json andThen jBoolPrism
  final def byte: Fold[Json, Byte] = json andThen jBytePrism
  final def short: Fold[Json, Short] = json andThen jShortPrism
  final def int: Fold[Json, Int] = json andThen jIntPrism
  final def long: Fold[Json, Long] = json andThen jLongPrism
  final def bigInt: Fold[Json, BigInt] = json andThen jBigIntPrism
  final def bigDecimal: Fold[Json, BigDecimal] = json andThen jBigDecimalPrism
  final def number: Fold[Json, JsonNumber] = json andThen jNumberPrism
  final def string: Fold[Json, String] = json andThen jStringPrism
  final def arr: Fold[Json, List[Json]] = json andThen jArrayPrism
  final def obj: Fold[Json, JsonObject] = json andThen jObjectPrism

  final def at(field: String): Fold[Json, Option[Json]] =
    json andThen jObjectPrism andThen At.at(field)

  final def selectDynamic(field: String): JsonFoldPath =
    JsonFoldPath(json andThen jObjectPrism andThen Index.index(field))

  final def applyDynamic(field: String)(i: Int): JsonFoldPath = selectDynamic(field).index(i)

  final def apply(i: Int): JsonFoldPath = index(i)

  final def index(i: Int): JsonFoldPath =
    JsonFoldPath(json andThen jArrayPrism andThen Index.index(i))

  final def each: JsonFoldPath =
    JsonFoldPath(json andThen jDescendants)

  final def filterByIndex(p: Int => Boolean): JsonFoldPath =
    JsonFoldPath(arr andThen FilterIndex.filterIndex(p))

  final def filterByField(p: String => Boolean): JsonFoldPath =
    JsonFoldPath(obj andThen FilterIndex.filterIndex(p))

  final def filter(p: Json => Boolean): JsonFoldPath =
    JsonFoldPath(json andThen OpticsHelper.select(p))

  final def as[A: DecodeJson]: Fold[Json, A] =
    json andThen OpticsHelper.parse
}

object OpticsHelper {

  /** Decode a value at the current location */
  def parse[A](implicit decode: DecodeJson[A]): Fold[Json, A] =
    new Fold[Json, A] {
      def foldMap[M](f: A => M)(json: Json)(implicit ev: Monoid[M]): M =
        decode.decodeJson(json).fold((_, _) => ev.empty, f)
    }

  /** Select if a value matches a predicate */
  def select[A](p: A => Boolean): Fold[A, A] =
    new Fold[A, A] {
      def foldMap[M](f: A => M)(a: A)(implicit ev: Monoid[M]): M =
        if (p(a)) f(a) else ev.empty
    }
}
