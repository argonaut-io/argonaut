package argonaut

import scala.deriving.Mirror

trait DecodeJsonMacro { self: DecodeJson.type =>
  inline def derive[A](using Mirror.ProductOf[A]): DecodeJson[A] =
    internal.Macros.derivedDecoder[A]
}
