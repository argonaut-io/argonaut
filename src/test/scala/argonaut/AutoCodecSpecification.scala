package argonaut

import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._, Scalaz._
import shapeless._

object AutoCodecSpecification extends Specification with ScalaCheck {
  import Argonaut._

  sealed trait AbstractBeanFactorySingletonProxy

  val jsonFred =
    Json(
      "Fred" := Json(
        "s" := "Flinstone",
        "n" := 40
      )
    )

  val jsonNpe =
    Json(
      "NPE" := Json(
        "s" := "beans"
      )
    )

  case class Fred(s: String, n: Int) extends AbstractBeanFactorySingletonProxy
  case class NPE(s: String) extends AbstractBeanFactorySingletonProxy

  object ABFSPEnc {
    import AutoEncodeJson._
    import AutoEncodeJson.auto._
    implicit val enc = implicitly[EncodeJson[AbstractBeanFactorySingletonProxy]]
  }

  object ABFSPDec {
    import AutoDecodeJson._
    import AutoDecodeJson.auto._
    implicit val dec = implicitly[DecodeJson[AbstractBeanFactorySingletonProxy]]
  }

  val fred: AbstractBeanFactorySingletonProxy = Fred("Flinstone", 40)
  val npe: AbstractBeanFactorySingletonProxy = NPE("beans")

  def is = "Generic Derivers" ^
    "Fred is some json" ! {
      import ABFSPEnc._
      fred.asJson must_== jsonFred
    } ^
    "An NPE is some json" ! {
      import ABFSPEnc._
      npe.asJson must_== jsonNpe
    } ^
    "Some json is a fred" ! {
      import ABFSPDec._
      jsonFred.as[AbstractBeanFactorySingletonProxy] must_== DecodeResult(\/-(fred))
    } ^
    "Some json isn't fred" ! {
      import ABFSPDec._
      Json("banana" := true).as[AbstractBeanFactorySingletonProxy] must_== DecodeResult(-\/(("CNil", CursorHistory(List()))))
    } ^
    "The round trip is identity" ! {
      // quickcheck some data structures
      // derive implicits for them
      // check encode . decode == id
      false
    }.pendingUntilFixed

}
