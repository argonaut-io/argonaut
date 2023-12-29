package argonaut

import scalaz._
import std.string._

object DecodeJsonScalaz extends DecodeJsonScalazs {}

trait DecodeJsonScalazs {
  implicit def MaybeDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Maybe[A]] = {
    implicitly[DecodeJson[Option[A]]].map(Maybe.fromOption)
  }

  implicit def ScalazEitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[A \/ B] = {
    implicitly[DecodeJson[Either[A, B]]].map(\/.fromEither(_))
  }

  implicit def ValidationDecodeJson[A, B](implicit
    ea: DecodeJson[A],
    eb: DecodeJson[B]
  ): DecodeJson[Validation[A, B]] = {
    DecodeJson(a => {
      val l = (a --\ "Failure").success
      val r = (a --\ "Success").success
      (l, r) match {
        case (Some(c), None) => ea(c).map(Failure(_))
        case (None, Some(c)) => eb(c).map(Success(_))
        case _ => DecodeResult.fail("[A, B]Validation[A, B]", a.history)
      }
    })
  }

  implicit def IMapDecodeJson[A: DecodeJson]: DecodeJson[String ==>> A] = {
    DecodeJson.MapDecodeJson[String, A].map(a => ==>>.fromList(a.toList)).setName("[A]==>>[String, A]")
  }

  implicit def IListDecodeJson[A: DecodeJson]: DecodeJson[IList[A]] = {
    implicitly[DecodeJson[List[A]]].map(IList.fromList).setName("[A]IList[A]")
  }

  implicit def DListDecodeJson[A: DecodeJson]: DecodeJson[DList[A]] = {
    implicitly[DecodeJson[List[A]]].map(DList.fromList(_)).setName("[A]DList[A]")
  }

  implicit def EphemeralStreamDecodeJson[A: DecodeJson]: DecodeJson[EphemeralStream[A]] = {
    implicitly[DecodeJson[List[A]]].map(list => EphemeralStream.apply(list*)).setName("[A]EphemeralStream[A]")
  }

  implicit def ISetDecodeJson[A: DecodeJson: Order]: DecodeJson[ISet[A]] = {
    implicitly[DecodeJson[List[A]]].map(ISet.fromList(_)).setName("[A]ISet[A]")
  }

  implicit def NonEmptyListDecodeJson[A: DecodeJson]: DecodeJson[NonEmptyList[A]] = {
    implicitly[DecodeJson[List[A]]].flatMap(l =>
      DecodeJson[NonEmptyList[A]](c =>
        std.list.toNel(l) match {
          case Maybe.Empty() => DecodeResult.fail("[A]NonEmptyList[A]", c.history)
          case Maybe.Just(n) => DecodeResult.ok(n)
        }
      )
    ) setName "[A]NonEmptyList[A]"
  }
}
