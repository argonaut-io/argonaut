package argonaut

trait EncodeJsonMacro { self: EncodeJson.type =>
  inline def derive[A]: EncodeJson[A] =
    internal.Macros.summonEncoder[A]
}
