package argonaut

import scalaz._, Scalaz._
import shapeless._
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._
import Argonaut._

sealed trait Shape
case class Circle(radius: Int) extends Shape
case class Square(side: Int) extends Shape

object DecodeJsonSpecification extends Specification with ScalaCheck { def is = s2"""

DecodeJson Witness Compilation
------------------------------

  Witness basics                        ${ok}
  Witness tuples                        ${ok}
  Witness auto                          ${ok}
  Witness derived                       ${ok}

DecodeJson Auto Derivation
--------------------------

  Product-types correspond              ${auto.products}
  Sum-types correspond                  ${auto.sums}

"""

  object primitives {
    DecodeJson.of[String]
    DecodeJson.of[Int]
    DecodeJson.of[Boolean]
    DecodeJson.of[Long]
    DecodeJson.of[Double]
    DecodeJson.of[Short]
    DecodeJson.of[Option[Int]]
    DecodeJson.of[Option[String]]
  }

  object tuples {
    DecodeJson.of[(String, Int)]
    DecodeJson.of[(String, Int, Boolean)]
    DecodeJson.of[(String, Int, Boolean, Long)]
    DecodeJson.of[(String, Int, Boolean, Long, Double)]
  }



  object auto {
    import TestTypes._

    DecodeJson.of[Product]
    DecodeJson.of[OrderLine]
    DecodeJson.of[Order]
    DecodeJson.of[Person]

    def products = prop((p: Person) =>
      Json(
        "name" := p.name,
        "age" := p.age,
        "orders" := p.orders,
        "addressFields" := p.addressFields
      ).as[Person].toOption must_== Some(p))

    DecodeJson.of[Shape]

    def sums = prop((s: Shape) =>
      (s match {
        case Circle(radius) => Json("Circle" := Json("radius" := radius))
        case Square(side) => Json("Square" := Json("side" := side))
      }).as[Shape].toOption must_== Some(s))
  }
}
