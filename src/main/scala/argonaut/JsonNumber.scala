package argonaut

import java.math.MathContext

import monocle.{Iso, Prism}

import scalaz.Equal
import scalaz.syntax.std.boolean._

/**
 * JSON numbers with optimization by cases.
 * Note: Javascript numbers are 64-bit decimals.
 */
sealed abstract class JsonNumber {
  import Json._

  /**
   * Returns this number as a `BigDecimal`.
   */
  def toBigDecimal: BigDecimal

  /**
   * Converts this number to the best `Double` approximation to this number.
   * Anything over `Double.MaxValue` gets rounded to `Double.PositiveInfinity`
   * and anything below `Double.MinValue` gets rounded to
   * `Double.NegativeInfinity`.
   */
  def toDouble: Double

  /**
   * Converts this number to the best `Float` approximation to this number.
   * Anything over `Float.MaxValue` gets rounded to `Float.PositiveInfinity`
   * and anything below `Float.MinValue` gets rounded to
   * `Float.NegativeInfinity`.
   */
  def toFloat: Float = toDouble.toFloat

  /**
   * Returns this number as a `BigInt`, only if this number is an integer.
   */
  def toBigInt: Option[BigInt] = {
    val n = toBigDecimal
    if (n.isWhole) Some(n.toBigInt)
    else None
  }

  /**
   * Returns this number as a `Long`, only if this number is a valid `Long`.
   */
  def toLong: Option[Long]

  /**
   * Returns this number as a `Int`, only if this number is a valid `Int`.
   */
  def toInt: Option[Int] = toLong flatMap { n =>
    val m = n.toInt
    if (n == m) Some(m) else None
  }

  /**
   * Returns this number as a `Short`, only if this number is a valid `Short`.
   */
  def toShort: Option[Short] = toLong flatMap { n =>
    val m = n.toShort
    if (n == m) Some(m) else None
  }

  /**
   * Returns this number as a `Byte`, only if this number is a valid `Byte`.
   */
  def toByte: Option[Byte] = toLong flatMap { n =>
    val m = n.toByte
    if (n == m) Some(m) else None
  }

  /**
   * Truncates the number to a BigInt. Truncation means that we round the real
   * number towards 0 to the closest BigInt.
   */
  def truncateToBigInt: BigInt =  toBigDecimal.toBigInt

  /**
   * Truncates the number to a Long. Truncation means that we round the real
   * number towards 0 to the closest, valid Long. So, if the number is 1e99,
   * then this will return `Long.MaxValue`.
   */
  def truncateToLong: Long

  /**
   * Truncates the number to a Int. Truncation means that we round the real
   * number towards 0 to the closest, valid Int. So, if the number is 1e99,
   * then this will return `Int.MaxValue`.
   */
  def truncateToInt: Int = toDouble.toInt

  /**
   * Truncates the number to a Short. Truncation means that we round the real
   * number towards 0 to the closest, valid Short. So, if the number is 1e99,
   * then this will return `Short.MaxValue`.
   */
  def truncateToShort: Short = {
    val n = truncateToInt
    if (n > Short.MaxValue) Short.MaxValue
    else if (n < Short.MinValue) Short.MinValue
    else n.toShort
  }

  /**
   * Truncates the number to a Byte. Truncation means that we round the real
   * number towards 0 to the closest, valid Byte. So, if the number is 1e99,
   * then this will return `Byte.MaxValue`.
   */
  def truncateToByte: Byte = {
    val n = truncateToInt
    if (n > Byte.MaxValue) Byte.MaxValue
    else if (n < Byte.MinValue) Byte.MinValue
    else n.toByte
  }

  /**
   * Returns `true` iff this number wraps a `Double` and it is `NaN`.
   */
  def isNaN: Boolean = false

  /**
   * Returns `true` iff this number wraps a `Double` and it is
   * `PositiveInfinity` or `NegativeInfinity`.
   */
  def isInfinity: Boolean = false

  /**
   * Returns true if this is a valid real number (ie. `!(isNaN || isInfinity)`).
   */
  def isReal: Boolean = !(isNaN || isInfinity)

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

  // Force this `JsonNumber` into a `JsonDecimal` by using the underlying
  // numbers toString. Isn't safe is `isReal` is `false.
  private def toJsonDecimal: JsonDecimal = this match {
    case n @ JsonDecimal(_) => n
    case JsonBigDecimal(n) => JsonDecimal(n.toString)
    case JsonLong(n) => JsonDecimal(n.toString)
    case JsonDouble(n) => JsonDecimal(n.toString)
  }

  override def hashCode: Int =
    if (isReal) toJsonDecimal.normalized.hashCode
    else toDouble.hashCode

  override def equals(that: Any): Boolean = that match {
    case (that: JsonNumber) =>
      if (this.isReal && that.isReal) {
        (this, that) match {
          case (a @ JsonDecimal(_), b) => a.normalized == b.toJsonDecimal.normalized
          case (a, b @ JsonDecimal(_)) => a.toJsonDecimal.normalized == b.normalized
          case (JsonLong(x), JsonLong(y)) => x == y
          case (JsonDouble(x), JsonLong(y)) => x == y
          case (JsonLong(x), JsonDouble(y)) => y == x
          case (JsonDouble(x), JsonDouble(y)) => x == y
          case (a, b) => a.toBigDecimal == b.toBigDecimal
        }
      } else {
        this.toDouble == that.toDouble
      }

    case _ =>
      false
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
  lazy val toBigDecimal: BigDecimal = BigDecimal(value, MathContext.UNLIMITED)
  lazy val toDouble: Double = value.toDouble

  def toLong: Option[Long] = {
    val n = toBigDecimal
    if (n.isValidLong) Some(n.toLong)
    else None
  }

  def truncateToLong: Long = {
    val n = toBigDecimal
    if (n >= Long.MaxValue) Long.MaxValue
    else if (n <= Long.MinValue) Long.MinValue
    else n.toLong
  }

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
   * exponent will always be 0 as well.
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
      else None

    val unscaledExponent = Option(expStr) match {
      case Some(exp) if exp.startsWith("+") => BigInt(exp.substring(1))
      case Some(exp) => BigInt(exp)
      case None => BigInt(0)
    }
    rescale match {
      case Some(shift) =>
        val unscaledValue =
          if (decStr == null) BigDecimal(intStr, MathContext.UNLIMITED)
          else BigDecimal(s"$intStr.$decStr", MathContext.UNLIMITED)
        val scaledValue = BigDecimal(unscaledValue.bigDecimal.movePointLeft(shift))
        (unscaledExponent + shift, if (negative != null) -scaledValue else scaledValue)

      case None =>
        (BigInt(0), BigDecimal(0))
    }
  }
}

case class JsonBigDecimal(value: BigDecimal) extends JsonNumber {
  def toBigDecimal = value

  def toDouble = value.toDouble

  def toLong: Option[Long] = {
    if (value.isValidLong) Some(value.toLong)
    else None
  }

  def truncateToLong: Long =
    if (value > Long.MaxValue) Long.MaxValue
    else if (value < Long.MinValue) Long.MinValue
    else value.toLong
}

case class JsonLong(value: Long) extends JsonNumber {
  def toBigDecimal = BigDecimal(value)
  def toDouble = value.toDouble
  def toLong: Option[Long] = Some(value)
  def truncateToLong: Long = value
}

case class JsonDouble(value: Double) extends JsonNumber {
  def toBigDecimal = BigDecimal(value)
  def toDouble = value
  def toLong: Option[Long] = {
    val n = value.toLong
    if (n.toDouble == value) Some(n)
    else None
  }
  def truncateToLong: Long = value.toLong
  override def isNaN = value.isNaN
  override def isInfinity = value.isInfinity
}

object JsonNumber {
  implicit val JsonNumberEqual: Equal[JsonNumber] = new Equal[JsonNumber] {
    def equal(a: JsonNumber, b: JsonNumber) = a == b
  }

  val jNumberToBigDecimal: Iso[JsonNumber, BigDecimal] =
    Iso[JsonNumber, BigDecimal](_.toBigDecimal)(JsonBigDecimal.apply)

  val jNumberToDouble: Prism[JsonNumber, Double] =
    Prism[JsonNumber, Double]{n =>
      val d = n.toDouble
      if(JsonDouble(d) == n) Some(d)
      else None
    }(JsonDouble.apply)

  val jNumberToFloat: Prism[JsonNumber, Float] =
    Prism[JsonNumber, Float]{n =>
      val f = n.toFloat
      if(JsonDouble(f) == n) Some(f)
      else None
    }(f => JsonDouble(f))

  val jNumberToBigInt: Prism[JsonNumber, BigInt] =
    Prism[JsonNumber, BigInt](_.toBigInt)(bi => JsonBigDecimal(BigDecimal(bi, MathContext.UNLIMITED)))

  val jNumberToLong: Prism[JsonNumber, Long] =
    Prism[JsonNumber, Long](_.toLong)(JsonBigDecimal.apply _ compose BigDecimal.apply)

  val jNumberToInt: Prism[JsonNumber, Int] =
    Prism[JsonNumber, Int](_.toInt)(JsonBigDecimal.apply _ compose BigDecimal.apply)

  val jNumberToShort: Prism[JsonNumber, Short] =
    Prism[JsonNumber, Short](_.toShort)(s => JsonBigDecimal(BigDecimal(s)))

  val jNumberToByte: Prism[JsonNumber, Byte] =
    Prism[JsonNumber, Byte](_.toByte)(b => JsonBigDecimal(BigDecimal(b)))

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

trait EncodeJsonNumber[T] {
  def encodeJsonNumber(value: T): JsonNumber
}

object EncodeJsonNumber {
  implicit val encodeJsonNumberShort: EncodeJsonNumber[Short] = new EncodeJsonNumber[Short] {
    def encodeJsonNumber(value: Short): JsonNumber = new JsonLong(value.toLong)
  }
  implicit val encodeJsonNumberInt: EncodeJsonNumber[Int] = new EncodeJsonNumber[Int] {
    def encodeJsonNumber(value: Int): JsonNumber = new JsonLong(value.toLong)
  }
  implicit val encodeJsonNumberLong: EncodeJsonNumber[Long] = new EncodeJsonNumber[Long] {
    def encodeJsonNumber(value: Long): JsonNumber = new JsonLong(value)
  }
  implicit val encodeJsonNumberDouble: EncodeJsonNumber[Double] = new EncodeJsonNumber[Double] {
    def encodeJsonNumber(value: Double): JsonNumber = new JsonDouble(value)
  }
  implicit val encodeJsonNumberBigDecimal: EncodeJsonNumber[BigDecimal] = new EncodeJsonNumber[BigDecimal] {
    def encodeJsonNumber(value: BigDecimal): JsonNumber = new JsonBigDecimal(value)
  }
}
