package argonaut

import java.math.MathContext

import monocle.{Iso, Prism}

import scalaz.Equal
import scalaz.syntax.std.boolean._


object JsonNumberMonocle {
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

  val jNumberToBigDecimal: Iso[JsonNumber, BigDecimal] =
    Iso[JsonNumber, BigDecimal](_.toBigDecimal)(JsonBigDecimal.apply)
}
