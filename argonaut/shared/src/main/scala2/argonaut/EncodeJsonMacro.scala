package argonaut

import scala.language.experimental.macros

trait EncodeJsonMacro { self: EncodeJson.type =>
  def derive[A]: EncodeJson[A] = macro internal.Macros.materializeEncodeImpl[A]
}
