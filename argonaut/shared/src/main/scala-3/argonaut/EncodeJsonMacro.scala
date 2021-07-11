package argonaut

import scala.deriving.Mirror

trait EncodeJsonMacro { self: EncodeJson.type =>
  inline def derive[A](using Mirror.ProductOf[A]): EncodeJson[A] =
    internal.Macros.derivedEncoder[A]
}
