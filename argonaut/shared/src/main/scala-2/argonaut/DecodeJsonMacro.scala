package argonaut

import scala.language.experimental.macros

trait DecodeJsonMacro { self: DecodeJson.type =>
  def derive[A]: DecodeJson[A] = macro internal.Macros.materializeDecodeImpl[A]
}
