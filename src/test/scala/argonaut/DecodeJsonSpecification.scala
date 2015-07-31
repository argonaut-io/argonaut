package argonaut

import scalaz._, Scalaz._
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._
import Argonaut._
import DecodeResultMatchers._

object DecodeJsonSpecification extends Specification with ScalaCheck { def is = s2"""
  DecodeJson Witness Compilation
    Witness basics                        ${ok}
    Witness tuples                        ${ok}
    Witness auto                          ${ok}
    Witness derived                       ${ok}

  DecodeJson derive
    BackTicks                             ${derived.testBackTicksDecodeJson}

  From String parser
    successful parse                      ${successfulParse}
    failed parse                          ${failedParse}
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

  def successfulParse = prop((i: Int) =>
    decodeFromParser.decodeJson(Parse.parse(s""""$i"""").toOption.get) must beOk)

  def failedParse = prop((s: String) =>
    decodeFromParser.decodeJson(Json.jString(s+"x")) must beErrorWithMessage("can't parse"))

  val parseIntDecodeJson: String => String \/ Int = (s: String) =>
    try s.toInt.right
    catch { case t: Throwable => s"can't parse $s: ${t.getMessage}".left }

  val decodeFromParser =
    DecodeJson.fromParser(parseIntDecodeJson)

}
