package argonaut

import scalaz._, Scalaz._
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._
import Argonaut._

object DecodeJsonSpecification extends Specification with ScalaCheck { def is = s2"""
  DecodeJson Witness Compilation
    Witness basics                        ${ok}
    Witness tuples                        ${ok}
    Witness auto                          ${ok}
    Witness derived                       ${ok}
  DecodeJson derive
    BackTicks                             ${derived.testBackTicksDecodeJson}
"""

  object primitives {
    DecodeJson.of[String]
    DecodeJson.of[Int]
    DecodeJson.of[Boolean]
    DecodeJson.of[Long]
    DecodeJson.of[Double]
    DecodeJson.of[Short]
    DecodeJson.of[Byte]
    DecodeJson.of[Option[Int]]
    DecodeJson.of[Option[String]]
  }

  object tuples {
    DecodeJson.of[(String, Int)]
    DecodeJson.of[(String, Int, Boolean)]
    DecodeJson.of[(String, Int, Boolean, Long)]
    DecodeJson.of[(String, Int, Boolean, Long, Double)]
  }

  object derived {
    import TestTypes._

    implicit def ProductDecodeJson: DecodeJson[Product] = DecodeJson.derive[Product]
    implicit def OrderLineDecodeJson: DecodeJson[OrderLine] = DecodeJson.derive[OrderLine]
    implicit def OrderDecodeJson: DecodeJson[Order] = DecodeJson.derive[Order]
    implicit def PersonDecodeJson: DecodeJson[Person] = DecodeJson.derive[Person]

    DecodeJson.of[Person]

    implicit def BackTicksDecodeJson: DecodeJson[BackTicks] = DecodeJson.derive[BackTicks]
    def testBackTicksDecodeJson = Parse.decodeEither[BackTicks]("""{"a.b.c": "test"}""") === BackTicks("test").right
  }
}
