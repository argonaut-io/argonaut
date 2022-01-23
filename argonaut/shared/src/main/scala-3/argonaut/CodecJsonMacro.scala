package argonaut

import scala.deriving.Mirror

trait CodecJsonMacro { self: CodecJson.type =>
  inline def derive[A: Mirror.ProductOf]: CodecJson[A] =
    internal.Macros.derivedCodec[A]
}
