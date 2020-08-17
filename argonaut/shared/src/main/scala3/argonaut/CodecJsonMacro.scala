package argonaut

trait CodecJsonMacro { self: CodecJson.type =>
  inline def derive[A]: CodecJson[A] =
    internal.Macros.summonCodec[A]
}
