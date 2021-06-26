package argonaut

import argonaut.TestCompat._
import scala.util.Try
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import Argonaut._

object JawnParserSpecification {
  case class Example(a: Int, b: Long, c: Double)

  val exampleCodecJson: CodecJson[Example] =
    casecodec3(Example.apply, (_: Example).asTuple)("a", "b", "c")

  implicit val exampleCaseClassArbitrary: Arbitrary[Example] = Arbitrary(
    for {
      a <- arbitrary[Int]
      b <- arbitrary[Long]
      c <- arbitrary[Double]
    } yield Example(a, b, c)
  )
}

class JawnParserSpecification extends ArgonautSpec {
  import JawnParserSpecification._
  import JawnParser.facade

  def is = s2"""
  The JawnParser
    correctly marshal case classes with Long values           $marshal
  """

  def marshal =
    prop { (e: Example) =>
      val jsonString: String = exampleCodecJson.encode(e).nospaces
      val json: Try[Json] = org.typelevel.jawn.Parser.parseFromString(jsonString)
      exampleCodecJson.decodeJson(json.get).toOption match {
        case None => ko("did not parse")
        case Some(example) => example must_== e
      }
    }
}
