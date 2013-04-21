package argonaut

import scalaz._, Scalaz._

sealed trait CodecJson[A] {
  val encoder: EncodeJson[A]
  val decoder: DecodeJson[A]
}

object CodecJson extends CodecJsons {
  def apply[A](encode: A => Json, decode: HCursor => DecodeResult[A]): CodecJson[A] =
    new CodecJson[A] {
      val encoder = EncodeJson(encode)
      val decoder = DecodeJson(decode)
    }
}

trait CodecJsons {
}
