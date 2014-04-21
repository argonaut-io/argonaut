package argonaut

object AutoCodecSpecification extends App {
  import AutoEncodeJson._
  import AutoEncodeJson.auto._
  import Argonaut._

  case class Fred(s: String, n: Int)

  implicitly[EncodeJson[Fred]]

  println(Fred("Flinstone", 40).asJson.spaces2)

}
