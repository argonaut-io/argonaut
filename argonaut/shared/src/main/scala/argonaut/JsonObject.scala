package argonaut

import Json._

/**
 * A mapping from field to JSON value that maintains insertion order.
 *
 * @author Tony Morris
 */
sealed abstract class JsonObject {
  /**
   * Convert to a map.
   */
  def toMap: Map[JsonField, Json]

  /**
   * Insert the given association.
   */
  def +(f: JsonField, j: Json): JsonObject

  /**
   * Append the given association.
   */
  def :+(fj: (JsonField, Json)): JsonObject

  /**
   * Prepend the given association.
   */
  def +:(fj: (JsonField, Json)): JsonObject

  /**
   * Remove the given field association.
   */
  def -(f: JsonField): JsonObject

  /**
   * Return the JSON value associated with the given field.
   */
  def apply(f: JsonField): Option[Json]

  /**
   * Transform all associated JSON values.
   */
  def withJsons(k: Json => Json): JsonObject

  /**
   * Returns true if there are no associations.
   */
  def isEmpty: Boolean

  /**
   * Returns true if there is at least one association.
   */
  def isNotEmpty: Boolean

  /**
   * Returns true if there is an association with the given field.
   */
  def ??(f: JsonField): Boolean

  /**
   * Returns the list of associations in insertion order.
   */
  def toList: List[JsonAssoc]

  /**
   * Returns all associated values in insertion order.
   */
  def values: List[Json]

  /**
   * Returns all association keys in insertion order.
   */
  def fields: List[JsonField]

  /**
   * Returns all association keys in arbitrary order.
   */
  def fieldSet: Set[JsonField]

  /**
   * Map Json values.
   */
  def map(f: Json => Json): JsonObject

  /**
   * Returns the number of associations.
   */
  def size: Int
}

private[argonaut] case class JsonObjectInstance(
  fieldsMap: Map[JsonField, Json] = Map.empty,
  orderedFields: Vector[JsonField] = Vector.empty
) extends JsonObject {

  def toMap: Map[JsonField, Json] = fieldsMap

  def +(f: JsonField, j: Json): JsonObject = {
    if (fieldsMap.contains(f)) {
      copy(fieldsMap = fieldsMap.updated(f, j))
    } else {
      copy(fieldsMap = fieldsMap.updated(f, j), orderedFields = orderedFields :+ f)
    }
  }

  def :+(fj: (JsonField, Json)): JsonObject = {
    this.+(fj._1, fj._2)
  }

  def +:(fj: (JsonField, Json)): JsonObject = {
    val (f, j) = fj
    if (fieldsMap.contains(f))
      copy(fieldsMap = fieldsMap.updated(f, j))
    else
      copy(fieldsMap = fieldsMap.updated(f, j), orderedFields = f +: orderedFields)
  }

  def -(f: JsonField): JsonObject =
    copy(fieldsMap = fieldsMap - f, orderedFields = orderedFields.filterNot(_ == f))

  def apply(f: JsonField): Option[Json] = fieldsMap.get(f)

  def withJsons(k: Json => Json): JsonObject = map(k)

  def isEmpty: Boolean = fieldsMap.isEmpty

  def isNotEmpty: Boolean = !isEmpty

  def ??(f: JsonField): Boolean = fieldsMap.contains(f)

  def toList: List[(JsonAssoc)] = orderedFields.map(field => (field, fieldsMap(field))).toList

  def values: List[Json] = orderedFields.map(field => fieldsMap(field)).toList

  def fields: List[JsonField] = orderedFields.toList

  def fieldSet: Set[JsonField] = orderedFields.toSet

  def map(f: Json => Json): JsonObject = copy(fieldsMap = fieldsMap.foldLeft(Map.empty[JsonField, Json]){case (acc, (key, value)) => acc.updated(key, f(value))})

  def size: Int = fields.size

  override def toString: String = {
    s"object[${fieldsMap.map(_.toString).mkString(",")}]"
  }

  override def equals(o: Any) = {
    o match {
      case JsonObjectInstance(otherMap, _) => fieldsMap == otherMap
      case _ => false
    }
  }

  override def hashCode = fieldsMap.hashCode
}

object JsonObject extends JsonObjects {
  /**
   * Construct an empty association.
   */
  def empty: JsonObject = JsonObjectInstance()
  /**
   * Construct with a single association.
   */
  def single(f: JsonField, j: Json): JsonObject = {
    JsonObject.empty + (f, j)
  }
  /**
    * Construct an object from a TraversableOnce instance.
    */
  def fromTraversableOnce(t: TraversableOnce[(JsonField, Json)]): JsonObject = {
    t.foldLeft(empty)(_ :+ _)
  }
}

trait JsonObjects {
}