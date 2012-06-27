package com.ephox
package argonaut

import scalaz._, Scalaz._

/**
 * A value representation a JSON number according to ECMA-262.
 *
 * @author Tony Morris
 */
sealed trait JsonNumber {
  val toDouble: Double

  def +(n: JsonNumber): JsonNumber =
    binary(_ + _, n)

  def -(n: JsonNumber): JsonNumber =
    binary(_ - _, n)

  def *(n: JsonNumber): JsonNumber =
    binary(_ * _, n)

  def compare(n: JsonNumber): Ordering =
    Order[Double].order(toDouble, n.toDouble)

  def abs: JsonNumber =
    JsonNumber(toDouble.abs)

  def unary_- : JsonNumber =
    JsonNumber(-toDouble)

  def signum: JsonNumber =
    JsonNumber(toDouble.signum)

  def round: JsonNumber =
    JsonNumber(toDouble.round)

  def ceil: JsonNumber =
    JsonNumber(toDouble.ceil)

  def floor: JsonNumber =
    JsonNumber(toDouble.floor)

  def toFloat: Float =
    toDouble.toFloat

  def toLong: Long =
    toDouble.toLong

  def toInt: Int =
    toDouble.toInt

  override def equals(a: Any): Boolean =
    a.isInstanceOf[JsonNumber] && { toDouble == a.asInstanceOf[JsonNumber].toDouble }

  override def toString: String =
    toDouble.toString

  private def binary[A](
    i: (Double, Double) => Double
  , n: JsonNumber
  ): JsonNumber =
    JsonNumber(i(toDouble, n.toDouble))
}

object JsonNumber extends JsonNumbers {
  def apply(d: Double): JsonNumber =
    new JsonNumber {
      val toDouble = d
    }
}

trait JsonNumbers {
  implicit val JsonNumberInstances: Order[JsonNumber] with Show[JsonNumber] with Monoid[JsonNumber] =
    new Order[JsonNumber] with Show[JsonNumber] with Monoid[JsonNumber] {
      def zero = JsonNumber(Monoid[Double].zero)
      def append(x: JsonNumber, y: => JsonNumber) =
        JsonNumber(Monoid[Double].append(x.toDouble, y.toDouble))
      def show(x: JsonNumber) =
        Show[Double].show(x.toDouble)
      def order(x: JsonNumber, y: JsonNumber) =
        x compare y
    }
}
