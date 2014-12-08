package argonaut

import scalaz._, Scalaz._
import shapeless._
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._
import Argonaut._

object EncodeJsonSpecification extends Specification with ScalaCheck { def is = s2"""

EncodeJson Witness Compilation
------------------------------

  Witness basics                        ${ok}
  Witness tuples                        ${ok}
  Witness auto                          ${ok}
  Witness derived                       ${ok}

EncodeJson Auto Derivation
--------------------------

  Product-types correspond              ${auto.products}
  Sum-types correspond                  ${auto.sums}

"""

  object primitives {
    EncodeJson.of[String]
    EncodeJson.of[Int]
    EncodeJson.of[Boolean]
    EncodeJson.of[Long]
    EncodeJson.of[Double]
    EncodeJson.of[Short]
    EncodeJson.of[Option[Int]]
    EncodeJson.of[Option[String]]
  }

  object tuples {
    EncodeJson.of[(String, Int)]
    EncodeJson.of[(String, Int, Boolean)]
    EncodeJson.of[(String, Int, Boolean, Long)]
    EncodeJson.of[(String, Int, Boolean, Long, Double)]
  }

  object auto {
    import TestTypes._

    EncodeJson.of[Product]
    EncodeJson.of[OrderLine]
    EncodeJson.of[Order]
    EncodeJson.of[Person]

    def products = prop((p: Person) =>
      p.asJson must_== Json(
        "name" := p.name,
        "age" := p.age, 
        "orders" := p.orders, 
        "addressFields" := p.addressFields
      ))

    EncodeJson.of[Shape]

    def sums = prop((s: Shape) =>
      s.asJson must_== (s match {
        case Circle(radius) => Json("Circle" := Json("radius" := radius))
        case Square(side) => Json("Square" := Json("side" := side))
      }))
  }
}
