package argonaut

import CursorHistoryCats._
import cats._, data._
import ext.std.tuple._
import std.either._, std.string._
import syntax.contravariant._

object DecodeResultCats extends DecodeResultCatss {
}

trait DecodeResultCatss {

  @annotation.tailrec
  final def loop[A, X](d: DecodeResult[A], e: (String, CursorHistory) => X, f: A => Xor[X, DecodeResult[A]]): X = {
    if (d.isError) {
      e(d.message.get, d.history.get)
    } else {
      f(d.value.get) match {
        case Xor.Left(x) => x
        case Xor.Right(a) => loop(a, e, f)
      }
    }
  }

  type DecodeEither[A] = Either[(String, CursorHistory), A]

  implicit def DecodeResultEq[A](implicit EA: Eq[A]): Eq[DecodeResult[A]] =
    eitherEq[(String, CursorHistory), A].on(_.toEither)

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def pure[A](a: A) = DecodeResult.ok(a)
    def flatMap[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }

  implicit def DecodeResultShow[A](implicit SE: Show[DecodeEither[A]]): Show[DecodeResult[A]] =
    SE.contramap(_.toEither)
}
