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
    import shapeless._
    import EncodeJson.auto._
    import TestTypes._

    EncodeJson.of[Product]
    EncodeJson.of[OrderLine]
    EncodeJson.of[Order]
    EncodeJson.of[Person]

    def products = prop((p: Person) =>
      p.asJson must_== Json("Person" := Json("name" := p.name, "age" := p.age)))

    EncodeJson.of[Shape]

    def sums = prop((s: Shape) =>
      s.asJson must_== (s match {
        case Circle(radius) => Json("Circle" := Json("radius" := radius))
        case Square(side) => Json("Square" := Json("side" := side))
      }))
  }

  object derived {
    import TestTypes._

    implicit def ProductEncodeJson: EncodeJson[Product] = EncodeJson.derive[Product]
    implicit def OrderLineEncodeJson: EncodeJson[OrderLine] = EncodeJson.derive[OrderLine]
    implicit def OrderEncodeJson: EncodeJson[Order] = EncodeJson.derive[Order]
    implicit def PersonEncodeJson: EncodeJson[Person] = EncodeJson.derive[Person]

    EncodeJson.of[Person]
  }
}
