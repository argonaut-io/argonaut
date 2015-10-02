package argonaut

import scalaz._, syntax.equal._

object CodecJsonScalaz extends CodecJsonScalazs {
  trait CodecLaw {
    def encodedecode(a: A)(implicit A: Equal[A]) =
      decodeJson(encode(a)).value.exists (_ === a)
  }

  def codecLaw = new CodecLaw {}
}

trait CodecJsonScalazs {
}
