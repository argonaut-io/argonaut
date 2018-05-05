package argonaut

import org.scalacheck._

trait Data0 {

  implicit val ArbitraryUUID: Arbitrary[java.util.UUID] =
    Arbitrary(Gen.uuid)

}
