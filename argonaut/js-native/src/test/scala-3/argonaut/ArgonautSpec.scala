package argonaut

import org.specs2.*

trait ArgonautSpec extends Specification with ScalaCheck {
  implicit final class MustEqualExtension[A](a1: A) {
    def must_==(a2: A) = a1 must beEqualTo(a2)
  }
}
