package argonaut

import org.specs2.*
import org.specs2.scalacheck.Parameters

trait ArgonautSpec extends Specification with ScalaCheck {

  override implicit val defaultParameters: Parameters =
    Parameters(minTestsOk = 10, maxSize = 5)

}
