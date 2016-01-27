package argonaut

import cats._, data._, syntax.contravariant._, syntax.eq._, syntax.show._

object DecodeResultCats extends DecodeResultCatss {
}

trait DecodeResultCatss {
  import TupleInstances._
  
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

  implicit def DecodeResultEq[A](implicit EE: Eq[DecodeEither[A]]): Eq[DecodeResult[A]] =
    EE.on(_.toEither)

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def pure[A](a: A) = DecodeResult.ok(a)
    def flatMap[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }

  implicit def DecodeResultShow[A](implicit SE: Show[DecodeEither[A]]): Show[DecodeResult[A]] =
    SE.contramap(_.toEither)
}

private object TupleInstances extends TupleInstances0

private trait TupleInstances0 {

  implicit def Tuple2Eq[A, B](implicit EA: Eq[A], EB: Eq[B]): Eq[(A, B)] = new Eq[(A, B)] {
    override def eqv(x: (A, B), y: (A, B)): Boolean = x._1 === y._1 && x._2 === y._2
  }

  implicit def Tuple2Show[A, B](implicit SA: Show[A], SB: Show[B]): Show[(A, B)] = new Show[(A, B)] {
    override def show(f: (A, B)): String = "(" + f._1.show + ", " + f._2.show + "}"
  }
}
