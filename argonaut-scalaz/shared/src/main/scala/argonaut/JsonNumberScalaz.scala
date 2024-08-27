package argonaut

import scalaz.*

object JsonNumberScalaz {
  implicit val JsonNumberEqual: Equal[JsonNumber] =
    _ == _
}
