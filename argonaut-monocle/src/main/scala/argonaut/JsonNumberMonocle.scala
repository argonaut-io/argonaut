package argonaut

import java.math.MathContext

import monocle.{Iso, Prism}

object JsonNumberMonocle extends JsonNumberMonocles

trait JsonNumberMonocles {
  // val jNumberToDouble: Optional[JsonNumber, Double] =
  //   Optional[JsonNumber, Double](_.toDouble)(d => n => d.asPossibleJsonNumber.getOrElse(n))

  // val jNumberToFloat: Optional[JsonNumber, Float] =
  //   Optional[JsonNumber, Float](_.toFloat)(f => n => f.asPossibleJsonNumber.getOrElse(n))

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

  val jNumberToBigDecimal: Iso[JsonNumber, BigDecimal] =
    Iso[JsonNumber, BigDecimal](_.toBigDecimal)(JsonBigDecimal.apply)
}
