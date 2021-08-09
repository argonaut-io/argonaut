package argonaut

import cats._

object JsonNumberCats {
  implicit val JsonNumberEq: Eq[JsonNumber] =
    _ == _
}
