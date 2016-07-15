package argonaut

import cats.data._

object DecodeJsonCats extends DecodeJsonCatss {
}

trait DecodeJsonCatss {
  implicit def NonEmptyListDecodeJson[A: DecodeJson](implicit DL: DecodeJson[List[A]]): DecodeJson[NonEmptyList[A]] = {
    DL.flatMap(l =>
      DecodeJson[NonEmptyList[A]](c => NonEmptyList.fromList(l) match {
        case None => DecodeResult.fail("[A]NonEmptyList[A]", c.history)
        case Some(n) => DecodeResult.ok(n)
      })) setName "[A]NonEmptyList[A]"
  }

  implicit def ValidatedDecodeJson[A, B](implicit DA: DecodeJson[A], DB: DecodeJson[B]): DecodeJson[Validated[A, B]] = {
    DecodeJson(a => {
      val l = (a --\ "Invalid").success
      val r = (a --\ "Valid").success
      (l, r) match {
        case (Some(c), None) => DA(c).map(Validated.Invalid(_))
        case (None, Some(c)) => DB(c).map(Validated.Valid(_))
        case _ => DecodeResult.fail("[A, B]Validated[A, B]", a.history)
      }
    })
  }

  implicit def XorDecodeJson[A: DecodeJson, B: DecodeJson](implicit DE: DecodeJson[Either[A, B]]): DecodeJson[Xor[A, B]] = {
    DE.map(Xor.fromEither(_))
  }
}
