package argonaut

import cats.*

object JsonNumberCats {
  implicit val JsonNumberEq: Eq[JsonNumber] =
    _ == _
}
