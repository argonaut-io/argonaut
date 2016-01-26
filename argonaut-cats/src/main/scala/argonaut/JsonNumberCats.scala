package argonaut

import cats._

object JsonNumberCats {
  implicit val JsonNumberEq: Eq[JsonNumber] = new Eq[JsonNumber] {
    def eqv(a: JsonNumber, b: JsonNumber) = a == b
  }
}
