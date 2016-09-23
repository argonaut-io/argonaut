package argonaut

import scalaz._

object JsonNumberScalaz {
  implicit val JsonNumberEqual: Equal[JsonNumber] = new Equal[JsonNumber] {
    def equal(a: JsonNumber, b: JsonNumber) = a == b
  }
}
