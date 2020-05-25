package argonaut

import scala.language.experimental.macros

trait CodecJsonMacro { self: CodecJson.type =>
  def derive[A]: CodecJson[A] = macro internal.Macros.materializeCodecImpl[A]
}
