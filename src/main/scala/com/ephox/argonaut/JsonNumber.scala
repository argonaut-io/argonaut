package com.ephox
package argonaut

import scalaz._, Scalaz._

sealed trait JsonNumber {
  def toEither: Either[BigInt, Double] =
    this match {
      case IntJsonNumber(i) => Left(i)
      case DoubleJsonNumber(d) => Right(d)
    }

  def +(n: JsonNumber): JsonNumber =
    binary((x, y) => IntJsonNumber(x + y), (x, y) => DoubleJsonNumber(x + y), n)

  def -(n: JsonNumber): JsonNumber =
    binary((x, y) => IntJsonNumber(x - y), (x, y) => DoubleJsonNumber(x - y), n)

  def *(n: JsonNumber): JsonNumber =
    binary((x, y) => IntJsonNumber(x * y), (x, y) => DoubleJsonNumber(x * y), n)

  def compare(n: JsonNumber): Ordering =
    binary(Order[BigInt].order(_, _), Order[Double].order(_, _), n)

  def abs: JsonNumber =
    this match {
      case IntJsonNumber(i) => IntJsonNumber(i.abs)
      case DoubleJsonNumber(d) => DoubleJsonNumber(d.abs)
    }

  def unary_- : JsonNumber =
    this match {
      case IntJsonNumber(i) => IntJsonNumber(-i)
      case DoubleJsonNumber(d) => DoubleJsonNumber(-d)
    }

  def signum: JsonNumber =
    this match {
      case IntJsonNumber(i) => IntJsonNumber(i.signum)
      case DoubleJsonNumber(d) => DoubleJsonNumber(d.signum)
    }

  def round: JsonNumber =
    this match {
      case IntJsonNumber(i) => IntJsonNumber(i)
      case DoubleJsonNumber(d) => DoubleJsonNumber(d.round)
    }

  def ceil: JsonNumber =
    this match {
      case IntJsonNumber(i) => IntJsonNumber(i)
      case DoubleJsonNumber(d) => DoubleJsonNumber(d.ceil)
    }

  def floor: JsonNumber =
    this match {
      case IntJsonNumber(i) => IntJsonNumber(i)
      case DoubleJsonNumber(d) => DoubleJsonNumber(d.floor)
    }

  def toDouble: Double =
    this match {
      case IntJsonNumber(i) => i.toDouble
      case DoubleJsonNumber(d) => d
    }

  def toFloat: Float =
    this match {
      case IntJsonNumber(i) => i.toFloat
      case DoubleJsonNumber(d) => d.toFloat
    }

  def toLong: Long =
    this match {
      case IntJsonNumber(i) => i.toLong
      case DoubleJsonNumber(d) => d.toLong
    }

  def toInt: Int =
    this match {
      case IntJsonNumber(i) => i.toInt
      case DoubleJsonNumber(d) => d.toInt
    }

  override def equals(a: Any): Boolean =
    a.isInstanceOf[JsonNumber] && binary(_ == _, _ == _, a.asInstanceOf[JsonNumber])

  override def toString: String =
    this match {
      case IntJsonNumber(i) => i.toString
      case DoubleJsonNumber(d) => d.toString
    }

  private def binary[A](
    i: (BigInt, BigInt) => A
  , d: (Double, Double) => A
  , n: JsonNumber
  ): A =
    this match {
      case IntJsonNumber(i1) => n match {
        case IntJsonNumber(i2) => i(i1, i2)
        case DoubleJsonNumber(d2) => i(i1, d2.toInt)
      }
      case DoubleJsonNumber(d1) => n match {
        case IntJsonNumber(i2) => i(d1.toInt, i2)
        case DoubleJsonNumber(d2) => d(d1, d2)
      }
    }
}
private case class DoubleJsonNumber(d: Double) extends JsonNumber
private case class IntJsonNumber(i: BigInt) extends JsonNumber

object JsonNumber extends JsonNumbers {
  def apply(d: Double): JsonNumber =
    DoubleJsonNumber(d)
}

trait JsonNumbers {
  def jIntegralNumber(i: BigInt): JsonNumber =
    IntJsonNumber(i)

  val jDoubleL: JsonNumber @?> Double =
    PLens {
      case DoubleJsonNumber(d) => Some(Store(DoubleJsonNumber(_), d))
      case IntJsonNumber(_) => None
    }

  val jIntegralL: JsonNumber @?> BigInt =
    PLens {
      case DoubleJsonNumber(_) => None
      case IntJsonNumber(i) => Some(Store(IntJsonNumber(_), i))
    }

  implicit val JsonNumberInstances: Order[JsonNumber] with Show[JsonNumber] with Monoid[JsonNumber] =
    new Order[JsonNumber] with Show[JsonNumber] with Monoid[JsonNumber] {
      def zero = DoubleJsonNumber(0D)
      def append(x: JsonNumber, y: => JsonNumber) =
        x + y
      def show(x: JsonNumber) =
        x match {
          case IntJsonNumber(i) => implicitly[Show[BigInt]].show(i)
          case DoubleJsonNumber(d) => implicitly[Show[Double]].show(d)
        }
      def order(x: JsonNumber, y: JsonNumber) =
        x compare y
    }
}
