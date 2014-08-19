package argonaut

import scalaz.{Each => _, Index => _, _}, Scalaz._

sealed trait JsonNumber{

  // unsafe implementation
  def toDouble = this match {
    case JsonNumberDouble(value) => value
    case JsonNumberLong(value) => value.toDouble
  }

  def toDoubleXX: Option[Double] = this match {
    case JsonNumberDouble(value) => Some(value)
    case JsonNumberLong(value) => 
      (value <= Double.MaxValue.toLong && value >= Double.MinValue.toLong).option(value.toDouble)
  }

  def floor = toDouble.floor

  def toInt = toDouble.toInt

  def toFloat = toDouble.toFloat
  
  def toLong = toDouble.toLong

  def toShort = toDouble.toShort
}

case class JsonNumberDouble(value: Double) extends JsonNumber

case class JsonNumberLong(value: Long) extends JsonNumber
