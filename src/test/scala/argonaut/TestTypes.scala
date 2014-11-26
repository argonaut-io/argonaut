package argonaut

import scalaz._, Scalaz._
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._

case class Product(name: String, price: Double)
case class OrderLine(product: Product, quantity: Int)
case class Order(orderLines: Vector[OrderLine])
case class Person(name: String, age: Int, orders: Vector[Order], addressFields: Map[String, String])

sealed trait Shape
case class Circle(radius: Int) extends Shape
case class Square(side: Int) extends Shape

object TestTypes {
  implicit def ProductArbitrary: Arbitrary[Product] = Arbitrary(for {
    n <- arbitrary[String]
    p <- arbitrary[Double]
  } yield Product(n, p))

  implicit def OrderLineArbitrary: Arbitrary[OrderLine] = Arbitrary(for {
    p <- arbitrary[Product]
    q <- arbitrary[Int]
  } yield OrderLine(p, q))

  implicit def OrderArbitrary: Arbitrary[Order] = Arbitrary(for {
    ol <- arbitrary[Vector[OrderLine]]
  } yield Order(ol))

  implicit def PersonArbitrary: Arbitrary[Person] = Arbitrary(for {
    n <- arbitrary[String]
    a <- arbitrary[Int]
    o <- arbitrary[Vector[Order]]
    af <- arbitrary[Map[String, String]]
  } yield Person(n, a, o, af))

  implicit def ShapeArbitrary: Arbitrary[Shape] = Arbitrary(Gen.oneOf(
    arbitrary[Int].map(Circle.apply)
  , arbitrary[Int].map(Square.apply)
  ))
}