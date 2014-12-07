package argonaut

import org.scalacheck._, Prop._, Arbitrary._, Gen._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._

import Argonaut._
import Data._

object JsonNumberSpecification extends Specification with ScalaCheck {
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
  """

  def longValuesProduceLongs = prop { (value: Long) =>
    JsonNumber.fromString(value.toString) must_== Some(JsonLong(value))
  }

  def parseValidJsonNumbers = prop { (num: ValidJsonNumber) =>
    JsonNumber.fromString(num.value) must beSome.like{
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
}
