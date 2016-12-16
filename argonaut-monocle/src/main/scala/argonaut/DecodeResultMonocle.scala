package argonaut

import monocle.std.either
import monocle.{Iso, Prism}

object DecodeResultMonocle extends DecodeResultMonocles

trait DecodeResultMonocles {
  def decodeResult[A]: Iso[DecodeResult[A], Either[(String, CursorHistory), A]] =
    Iso[DecodeResult[A], Either[(String, CursorHistory), A]](_.toEither)(DecodeResult(_))

  def success[A]: Prism[DecodeResult[A], A] =
    decodeResult composePrism either.stdRight

  def fail[A]: Prism[DecodeResult[A], (String, CursorHistory)] =
    decodeResult composePrism either.stdLeft
}
