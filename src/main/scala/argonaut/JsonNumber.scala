package argonaut

import scalaz.Equal
import scalaz.Scalaz._
import monocle.function._
import monocle.std._

/**
 * JSON numbers with optimization by cases.
 * Note: Javascript numbers are 64-bit decimals.
 */
sealed abstract class JsonNumber {
  import Json._

  def toDouble: Double
  def toFloat: Float
  def toInt: Int
  def toLong: Long
  def toShort: Short

  /** Safely coerce to an `Int` if this number fits in an `Int`, otherwise `None` */
  def safeInt: Option[Int] = safeCast[Double, Int].getOption(toDouble)

  /** Safely coerce to a `Long` if this number fits in a `Long`, otherwise `None` */
  def safeLong: Option[Long] = {
    val n = toDouble
    (n.floor == n) option toLong
  }

  def isNaN: Boolean = false
  def isInfinity: Boolean = false

  /**
   * Construct a JSON value that is a number.
   *
   * Note: NaN, +Infinity and -Infinity are not valid json.
   */
  def asJson: Option[Json] =
    (!isNaN && !isInfinity).option(JNumber(this))

  /**
   * Construct a JSON value that is a number. Transforming
   * NaN, +Infinity and -Infinity to jNull. This matches
   * the behaviour of most browsers, but is a lossy operation
   * as you can no longer distinguish between NaN and Infinity.
   */
  def asJsonOrNull: Json =
    asJson.getOrElse(jNull)

  /**
   * Construct a JSON value that is a number. Transforming
   * NaN, +Infinity and -Infinity to their string implementations.
   *
   * This is an argonaut specific transformation that allows all
   * doubles to be encoded without losing information, but aware
   * interoperability is unlikely without custom handling of
   * these values. See also `jNumber` and `jNumberOrNull`.
   */
  def asJsonOrString: Json =
    asJson.getOrElse(jString(toString))
}

case class JsonLong(value: Long) extends JsonNumber {
  def toDouble = value.toDouble
  def toFloat = value.toFloat
  def toInt = value.toInt
  def toLong = value
  def toShort = value.toShort
}
case class JsonDouble(value: Double) extends JsonNumber {
  def toDouble = value
  def toFloat = value.toFloat
  def toInt = value.toInt
  def toLong = value.toLong
  def toShort = value.toShort
  override def isNaN = value.isNaN
  override def isInfinity = value.isInfinity
}

object JsonNumber {
  implicit val JsonNumberEqual: Equal[JsonNumber] = new Equal[JsonNumber] {
    def equal(a: JsonNumber, b: JsonNumber) = a match {
      case JsonLong(n) => n == b.toLong
      case JsonDouble(n) => n == b.toDouble
    }
  }
}
