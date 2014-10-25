package argonaut

import scalaz._, Scalaz._
import shapeless._
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._

// Defining these below (close to ShapeArbitrary say) makes the DecodeJson derivation fail
// Same thing for EncodeJson
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
    import shapeless._
    import DecodeJson.auto._
    import StringWrap._

    case class Person(name: String, age: Int)

    implicit def PersonArbitrary: Arbitrary[Person] = Arbitrary(for {
      n <- arbitrary[String]
      a <- arbitrary[Int]
    } yield Person(n, a))

    DecodeJson.of[Person]

    def products = prop((p: Person) =>
      Json("Person" := Json("name" := p.name, "age" := p.age)).as[Person].toOption must_== Some(p))


    implicit def ShapeArbitrary: Arbitrary[Shape] = Arbitrary(Gen.oneOf(
      arbitrary[Int].map(Circle.apply)
    , arbitrary[Int].map(Square.apply)
    ))

    def sums = prop((s: Shape) =>
      (s match {
        case Circle(radius) => Json("Circle" := Json("radius" := radius))
        case Square(side) => Json("Square" := Json("side" := side))
      }).as[Shape].toOption must_== Some(s))
  }

  object derived {
    case class Person(name: String, age: Int)

    implicit def PersonDecodeJson: DecodeJson[Person] =
      DecodeJson.derive[Person]

    DecodeJson.of[Person]
  }

}
