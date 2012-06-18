package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

/**
 * A mapping from field to JSON value that maintains insertion order.
 *
 * @author Tony Morris
 */
sealed trait JsonObject {
  /**
   * Convert to an insertion map.
   */
  val toMap: InsertionMap[JsonField, Json]

  /**
   * Insert the given association.
   */
  def +(f: JsonField, j: Json): JsonObject =
    JsonObject(toMap ^+^ (f, j))

  /**
   * Remove the given field association.
   */
  def -(f: JsonField): JsonObject =
    JsonObject(toMap ^-^ f)

  /**
   * Return the JSON value associated with the given field.
   */
  def apply(f: JsonField): Option[Json] =
    toMap get f

  /**
   * Transform all associated JSON values.
   */
  def withJsons(k: Json => Json): JsonObject =
    JsonObject(toMap map k)

  /**
   * Returns true if there are no associations.
   */
  def isEmpty: Boolean =
    toMap.isEmpty

  /**
   * Returns true if there is at least one association.
   */
  def isNotEmpty: Boolean =
    !isEmpty

  /**
   * Returns true if there is an association with the given field.
   */
  def ??(f: JsonField): Boolean =
    toMap contains f

  /**
   * Returns the list of associations in insertion order.
   */
  def toList: List[(JsonField, Json)] =
    toMap.toList

  /**
   * Returns all associated values in insertion order.
   */
  def values: List[Json] =
    toList map (_._2)

  /**
   * Returns a kleisli function that gets the JSON value associated with the given field.
   */
  def kleisli: Kleisli[Option, JsonField, Json] =
    Kleisli(toMap get _)

  /**
   * Returns all association keys in insertion order.
   */
  def fields: List[JsonField] =
    toMap.keys

  /**
   * Returns the number of associations.
   */
  def size: Int =
    toMap.size

  override def toString: String =
    "object[" + (toMap.toList map (Show[(JsonField, Json)] shows _) mkString ", ") + "]"
}

object JsonObject extends JsonObjects {
  private[argonaut] def apply(x: InsertionMap[JsonField, Json]): JsonObject =
    new JsonObject {
      val toMap = x
    }
}

trait JsonObjects {
  /**
   * Construct an empty association.
   */
  def empty: JsonObject =
    JsonObject(InsertionMap.empty)

  /**
   * Construct with a single association.
   */
  def single(f: JsonField, j: Json): JsonObject =
    empty + (f, j)

  /**
   * The lens to the JSON value.
   */
  def jsonObjectL(f: JsonField): JsonObject @> Option[Json] =
    InsertionMap.insertionMapL(f).xmapA(JsonObject(_), _.toMap)

  /**
   * The partial lens to the JSON value.
   */
  def jsonObjectPL(f: JsonField): JsonObject @?> Json =
    PLensT.somePLens compose ~jsonObjectL(f)

  implicit val JsonObjectInstances: Equal[JsonObject] with Show[JsonObject] =
    new Equal[JsonObject] with Show[JsonObject] {
      def equal(j1: JsonObject, j2: JsonObject) =
        j1.toMap == j2.toMap
      def show(a: JsonObject) = Show.showFromToString show a

    }

}
