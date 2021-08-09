package argonaut

import scalaz._

object JsonNumberScalaz {
  implicit val JsonNumberEqual: Equal[JsonNumber] =
    _ == _
}
