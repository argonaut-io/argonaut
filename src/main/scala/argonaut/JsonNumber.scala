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

  def toJsonDecimal: JsonDecimal = this match {
    case n @ JsonDecimal(_) => n
    case JsonBigDecimal(n) => JsonDecimal(n.toString)
    case JsonLong(n) => JsonDecimal(n.toString)
    case JsonDouble(n) => JsonDecimal(n.toString)
  }

  override def hashCode: Int = toJsonDecimal.normalized.hashCode
  override def equals(that: Any): Boolean = that match {
    case (that: JsonNumber) => this === that
    case _ => false
  }
}

/**
 * A JsonDecimal represents and valid JSON number as a String. Unfortunately,
 * there is no type in the Scala standard library which can represent all valid
 * JSON decimal numbers, since the exponent may be larger than an `Int`. Such
 * a number can still be round tripped (parser to printer). We lazily parse the
 * string to a `BigDecimal` or a `Double` on demand.
 */
case class JsonDecimal private[argonaut] (value: String) extends JsonNumber {
  lazy val toBigDecimal: BigDecimal = BigDecimal(value)
  lazy val toDouble: Double = value.toDouble

  def toFloat = toDouble.toFloat
  def toLong = toBigDecimal.toLong
  def toInt = toDouble.toInt
  def toShort = toDouble.toShort

  /**
   * Returns a *normalized* version of this Decimal number. Since BigDecimal
   * cannot represent all valid JSON value exactly, due to the exponent being
   * limited to an `Int`, this method let's us get a normalized number that
   * can be used to compare for equality.
   *
   * The 1st value (BigInt) is the exponent used to scale the 2nd value
   * (BigDecimal) back to the original value represented by this number.  The
   * 2nd BigDecimal will always either be 0 or a number with exactly 1 decimal
   * digit to the right of the decimal point.  If the 2nd value is 0, then the
   * exponent (1st value) will be 1 of 3 values: 1, 0, or -1 (the sign of the
   * parsed exponent). This is so that we have 1 canonical value for 0,
   * undetermined, and infinity, resp.
   */
  def normalized: (BigInt, BigDecimal) = {
    val JsonNumber.JsonNumberRegex(negative, intStr, decStr, expStr) = value

    def decScale(i: Int): Option[Int] =
      if (i >= decStr.length) None
      else if (decStr(i) == '0') decScale(i + 1)
      else Some(- i - 1)

    val rescale =
      if (intStr != "0") Some(intStr.length - 1)
      else if (decStr != null) decScale(0)
      else Some(0)

    val unscaledExponent = Option(expStr).map(BigInt(_)).getOrElse(BigInt(0))
    rescale match {
      case Some(shift) =>
        val unscaledValue =
          if (decStr == null) BigDecimal(intStr)
          else BigDecimal(s"$intStr.$decStr")
        val scaledValue = BigDecimal(unscaledValue.bigDecimal.movePointLeft(shift))
        (unscaledExponent + shift, if (negative != null) -scaledValue else scaledValue)

      case None =>
        val exp =
          if (unscaledExponent < 0) -1
          else if (unscaledExponent > 0) 1
          else 0
        (BigInt(exp), BigDecimal(0))
    }
  }
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
      case (a @ JsonDecimal(_), _) => a.normalized == b.toJsonDecimal.normalized
      case (_, b @ JsonDecimal(_)) => a.toJsonDecimal.normalized == b.normalized
      case (JsonLong(x), JsonLong(y)) => x == y
      case (JsonDouble(x), JsonLong(y)) => x == y
      case (JsonLong(x), JsonDouble(y)) => y == x
      case (JsonDouble(x), JsonDouble(y)) => x == y
      case _ => a.toBigDecimal == b.toBigDecimal
    }
  }

  /**
   * Returns a `JsonNumber` whose value is the valid JSON number in `value`.
   * This value is **not** verified to be a valid JSON string. It is assumed
   * that `value` is a valid JSON number, according to the JSON specification.
   * If the value is invalid then the results are undefined. This is provided
   * for use in places like a Jawn parser facade, which provides its own
   * verification of JSON numbers.
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

  /**
   * A regular expression that can match a valid JSON number. This has 4 match
   * groups:
   *
   *  1. The optional negative sign.
   *  2. The integer part.
   *  3. The fractional part without the leading period.
   *  4. The exponent part without the leading 'e', but with an optional leading '+' or '-'.
   *
   * The negative sign, fractional part and exponent part are optional matches
   * and may be `null`.
   */
  val JsonNumberRegex = 
    """(-)?((?:[1-9][0-9]*|0))(?:\.([0-9]+))?(?:[eE]([-+]?[0-9]+))?""".r
}
