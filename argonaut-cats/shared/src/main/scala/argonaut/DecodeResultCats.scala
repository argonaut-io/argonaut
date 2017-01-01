package argonaut

import CursorHistoryCats._
import cats._
import instances.tuple._
import instances.either._, instances.string._
import syntax.contravariant._

object DecodeResultCats extends DecodeResultCatss {
}

trait DecodeResultCatss {

  type DecodeEither[A] = Either[(String, CursorHistory), A]

  implicit def DecodeResultEq[A](implicit EA: Eq[A]): Eq[DecodeResult[A]] =
    Eq[DecodeEither[A]].on(_.toEither)

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def pure[A](a: A) = DecodeResult.ok(a)
    def flatMap[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def tailRecM[A, B](a: A)(f: A => DecodeResult[Either[A, B]]) =
      DecodeResult(Monad[DecodeEither].tailRecM(a)(f andThen (_.toEither)))
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }

  implicit def DecodeResultShow[A](implicit SE: Show[DecodeEither[A]]): Show[DecodeResult[A]] =
    SE.contramap(_.toEither)
}
