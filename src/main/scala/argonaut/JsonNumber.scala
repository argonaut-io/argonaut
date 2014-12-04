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

  def toBigDecimal: BigDecimal
  def toDouble: Double
  def toFloat: Float
  def toLong: Long
  def toInt: Int
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

case class JsonDecimal private[argonaut] (value: String) extends JsonNumber {
  lazy val toBigDecimal: BigDecimal = BigDecimal(value)
  lazy val toDouble: Double = value.toDouble

  def toFloat = toDouble.toFloat
  def toLong = toBigDecimal.toLong
  def toInt = toDouble.toInt
  def toShort = toDouble.toShort
}

case class JsonBigDecimal(value: BigDecimal) extends JsonNumber {
  def toBigDecimal = value
  def toDouble = value.toDouble
  def toFloat = value.toFloat
  def toInt = value.toInt
  def toLong = value.toLong
  def toShort = value.toShort

  override def safeInt: Option[Int] =
    if (value.isValidInt) Some(toInt) else None

  override def safeLong: Option[Long] =
    if (value.isValidLong) Some(toLong) else None
}

case class JsonLong(value: Long) extends JsonNumber {
  def toBigDecimal = BigDecimal(value)
  def toDouble = value.toDouble
  def toFloat = value.toFloat
  def toInt = value.toInt
  def toLong = value
  def toShort = value.toShort

  override def safeInt: Option[Int] = {
    if (value >= Int.MinValue && value <= Int.MaxValue) Some(value.toInt)
    else None
  }

  override def safeLong: Option[Long] = Some(value)
}

case class JsonDouble(value: Double) extends JsonNumber {
  def toBigDecimal = BigDecimal(value)
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
    def equal(a: JsonNumber, b: JsonNumber) = (a, b) match {
      case (JsonLong(x), JsonLong(y)) => x == y
      case (JsonDouble(x), JsonLong(y)) => x == y
      case (JsonLong(x), JsonDouble(y)) => y == x
      case (JsonDouble(x), JsonDouble(y)) => x == y
      case _ => a.toBigDecimal == b.toBigDecimal
    }
  }

  /**
   * Constructs a lazy `JsonNumber` whose value is ***not** verified. It is
   * assumed that `value` is a valid JSON number, according to the JSON
   * specification If the value is invalid then the results are undefined.
   */
  def unsafeDecimal(value: String): JsonNumber = JsonDecimal(value)

  /**
   * Parses a JSON number from a string. A String is valid if it conforms to
   * the grammar in the JSON specification (RFC 4627 -
   * http://www.ietf.org/rfc/rfc4627.txt), section 2.4. If it is valid, then
   * the number is returned in a `Some`. Otherwise the number is invalid and
   * `None` is returned.
   *
   * @param value a JSON number encoded as a string
   * @return a JSON number if the string is valid
   */
  def fromString(value: String): Option[JsonNumber] = {

    // Span over [0-9]*
    def digits(index: Int): Int = {
      if (index >= value.length) value.length
      else {
        val char = value(index)
        if (char >= '0' && char <= '9') digits(index + 1)
        else index
      }
    }

    // Verify [0-9]+
    def digits1(index: Int): Int = {
      val end = digits(index)
      if (end == index) -1
      else end
    }

    // Verify 0 | [1-9][0-9]*
    def natural(index: Int): Int = {
      if (index >= value.length) -1
      else {
        val char = value(index)
        if (char == '0') index + 1
        else if (char >= '1' && char <= '9') digits(index + 1)
        else index
      }
    }

    // Verify -?(0 | [1-9][0-9]*)
    def integer(index: Int): Int = {
      if (index >= value.length) -1
      else if (value(index) == '-') natural(index + 1)
      else natural(index)
    }

    // Span .[0-9]+
    def decimal(index: Int): Int = {
      if (index < 0 || index >= value.length) index
      else if (value(index) == '.') digits1(index + 1)
      else index
    }

    // Span e[-+]?[0-9]+
    def exponent(index: Int): Int = {
      if (index < 0 || index >= value.length) index
      else {
        val e = value(index)
        if (e == 'e' || e == 'E') {
          val index0 = index + 1
          if (index0 < value.length) {
            val sign = value(index0)
            if (sign == '+' || sign == '-') digits1(index0 + 1)
            else digits1(index0)
          } else {
            -1
          }
        } else {
          -1
        }
      }
    }

    val intIndex = integer(0)
    val decIndex = decimal(intIndex)
    val expIndex = exponent(decIndex)

    val invalid =
      (expIndex != value.length) ||
      (intIndex == 0) ||
      (intIndex == -1) ||
      (decIndex == -1)

    // Assuming the number is an integer, does it fit in a Long?
    def isLong: Boolean = {
      val upperBound = if (value(0) == '-') MinLongString else MaxLongString
      (value.length < upperBound.length) ||
        ((value.length == upperBound.length) &&
          value.compareTo(upperBound) <= 0)
    }

    if (invalid) {
      None
    } else if (intIndex == expIndex && isLong) {
      Some(JsonLong(value.toLong))
    } else {
      Some(JsonDecimal(value))
    }
  }

  private val MaxLongString = Long.MaxValue.toString
  private val MinLongString = Long.MinValue.toString
}
