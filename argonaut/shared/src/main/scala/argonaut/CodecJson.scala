package argonaut

sealed abstract class CodecJson[A] extends EncodeJson[A] with DecodeJson[A] { outer =>
  val Encoder: EncodeJson[A]
  val Decoder: DecodeJson[A]

  override def encode(a: A) = Encoder.encode(a)
  override def decode(c: HCursor) = Decoder.decode(c)
  override def setName(n: String): CodecJson[A] = {
    new CodecJson[A] {
      val Encoder = outer.Encoder
      val Decoder = outer.Decoder.setName(n)
    }
  }

  override def tryDecode(c: ACursor) = Decoder.tryDecode(c)

  def xmap[B](f: A => B)(g: B => A): CodecJson[B] = {
    CodecJson.derived(using Encoder.contramap(g), Decoder.map(f))
  }
}

object CodecJson extends CodecJsons with CodecJsonMacro {
  def apply[A](encoder: A => Json, decoder: HCursor => DecodeResult[A]): CodecJson[A] =
    derived(using EncodeJson(encoder), DecodeJson(decoder))

  def withReattempt[A](encoder: A => Json, decoder: ACursor => DecodeResult[A]): CodecJson[A] =
    derived(using EncodeJson(encoder), DecodeJson.withReattempt(decoder))

  def derived[A](implicit E: EncodeJson[A], D: DecodeJson[A]): CodecJson[A] = {
    new CodecJson[A] {
      val Encoder = E
      val Decoder = D
    }
  }

  def codecLaw[A](codec: CodecJson[A])(a: A): Boolean = {
    codec.decodeJson(codec.encode(a)).value.exists(_ == a)
  }
}

trait CodecJsons extends GeneratedCodecJsons
