package argonaut

trait DecodeJsonMacro { self: DecodeJson.type =>
  inline def derive[A]: DecodeJson[A] =
    internal.Macros.summonDecoder[A]
}
