package argonaut

import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._, Scalaz._
import shapeless._

object NestedAutoCodecSpecification extends Specification with ScalaCheck {
  import Argonaut._

  sealed trait T
  case class A(s: String) extends T
  case class B(a: A) extends T

  object TEnc {
    import AutoEncodeJson._
    // does not compile without first manually making
    // the codec for A.
    // it needs to be private to not conflict with the CodecJson[T]
    // at the use-site.
    private implicit val aEnc = AutoEncodeJson[A]
    implicit val tEnc = AutoEncodeJson[T]
  }

  object TDec {
    import AutoDecodeJson._
    //similar here
    private implicit val aDec = AutoDecodeJson[A]
    implicit val tDec = AutoDecodeJson[T]
  }

  val a: A = A("hello, world!")
  val b: B = B(a)

  val aJson: Json = 
    Json(
      "A" := Json(
        "s" := "hello, world!"
       )
    )

  val bJson: Json =
    Json(
      "B" := Json(
        "a" := aJson
      )
    )

  def is = "Nested Generic Derivers" ^
    "an 'A' is some json" ! {
      import TEnc._
      // having to do this is bad.
      // anyone know any magic tricks?
      (a: T).asJson must_== aJson
    } ^
    "a 'B' is some json" ! {
      import TEnc._
      (b: T).asJson must_== bJson
    } ^
    "some json is a 'B'" ! {
      import TDec._
      bJson.as[T] must_== DecodeResult(\/-(b))
    }
}
