package argonaut

import scalaz._, Scalaz._

sealed trait CodecJson[A] extends EncodeJson[A] with DecodeJson[A] {
  trait CodecLaw {
    def encodedecode(a: A)(implicit A: Equal[A]) =
      decodeJson(encode(a)).value.exists (_ === a)
  }

  def codecLaw = new CodecLaw {}
}

object CodecJson extends CodecJsons {
  def apply[A](encoder: A => Json, decoder: HCursor => DecodeResult[A]): CodecJson[A] =
    new CodecJson[A] {
      def encode(a: A) = encoder(a)
      def decode(c: HCursor) = decoder(c)
    }
}

trait CodecJsons extends GeneratedCodecJsons with LowPriorityCodecJsons

trait LowPriorityCodecJsons {
  implicit def DerivedCodecJson[A](implicit E: EncodeJson[A], D: DecodeJson[A]) =
    CodecJson(E.encode, D.decode)
}
