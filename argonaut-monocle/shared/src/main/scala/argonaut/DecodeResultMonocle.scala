package argonaut

import monocle.std.either
import monocle.{Iso, Prism}

object DecodeResultMonocle extends DecodeResultMonocles

trait DecodeResultMonocles {
  def decodeResult[A]: Iso[DecodeResult[A], Either[(String, CursorHistory), A]] =
    Iso[DecodeResult[A], Either[(String, CursorHistory), A]](_.toEither)(DecodeResult(_))

  def success[A]: Prism[DecodeResult[A], A] =
    decodeResult andThen either.stdRight[(String, CursorHistory), A]

  def fail[A]: Prism[DecodeResult[A], (String, CursorHistory)] =
    decodeResult andThen either.stdLeft[(String, CursorHistory), A]
}
