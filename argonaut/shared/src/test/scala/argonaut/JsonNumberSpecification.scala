package argonaut

import org.scalacheck.*
import Arbitrary.*
import Data.*

object JsonNumberSpecification extends ArgonautSpec {
  def is = s2"""
    fromString should
      Parse valid JSON number.                  $parseValidJsonNumbers
      Parse to Long when value fits in a Long.  $longValuesProduceLongs
      Fail on empty string.                     $failOnEmptyString
      Fail on missing integer part.             $failOnMissingInteger
      Fail on trailing decimal point.           $failOnTrailingDecimal
      Fail on leading zero.                     $failOnLeadingZero
      Fail on trailing 'e'.                     $failOnTrailingE

    equals should
      Equivalent numbers are equal.             $equivalentNumbersAreEqual

    truncateToBigInt should
      Fail on numbers with too many digits.     $failOnLargeDecimalRepresentation
  """

  def longValuesProduceLongs = prop { (value: Long) =>
    JsonNumber.fromString(value.toString) must_== Some(JsonLong(value))
  }

  def parseValidJsonNumbers = prop { (num: ValidJsonNumber) =>
    val Some(x) = JsonNumber.fromString(num.value)

    x match {
      case JsonDecimal(value) => num.value must_== value
      case JsonLong(value) => num.value must_== value.toString
    }
  }

  def equivalentNumbersAreEqual = prop { (pair: EquivalentJsonNumberPair) =>
    val EquivalentJsonNumberPair(lhs, rhs) = pair
    lhs must_== rhs
  }

  def failOnEmptyString = JsonNumber.fromString("") must beNone

  def failOnMissingInteger = {
    JsonNumber.fromString(".012e100") must beNone
    JsonNumber.fromString(".1234") must beNone
  }

  def failOnTrailingDecimal = {
    JsonNumber.fromString("0.") must beNone
    JsonNumber.fromString("-12.") must beNone
    JsonNumber.fromString("13.e-100") must beNone
  }

  def failOnLeadingZero = {
    JsonNumber.fromString("0123") must beNone
    JsonNumber.fromString("-001") must beNone
  }

  def failOnTrailingE = {
    JsonNumber.fromString("1e") must beNone
    JsonNumber.fromString("1e+") must beNone
    JsonNumber.fromString("1e-") must beNone
    JsonNumber.fromString("1E") must beNone
    JsonNumber.fromString("1E+") must beNone
    JsonNumber.fromString("1E-") must beNone
  }

  def failOnLargeDecimalRepresentation = {
    JsonNumber.fromString("1e262144").map(_.truncateToBigInt) must_== Some(None)
    JsonNumber.fromString("-1e262144").map(_.truncateToBigInt) must_== Some(None)
  }
}
