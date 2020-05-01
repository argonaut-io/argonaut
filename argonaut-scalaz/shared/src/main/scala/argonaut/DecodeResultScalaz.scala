package argonaut

import scalaz._, Isomorphism._, Scalaz._
import CursorHistoryScalaz._

object DecodeResultScalaz extends DecodeResultScalazs {
}

trait DecodeResultScalazs {
  @annotation.tailrec
  final def loop[A, X](d: DecodeResult[A], e: (String, CursorHistory) => X, f: A => X \/ DecodeResult[A]): X = {
    if (d.isError) {
      e(d.message.get, d.history.get)
    } else {
      f(d.value.get) match {
        case -\/(x) => x
        case \/-(a) => loop(a, e, f)
      }
    }
  }

  def failedResultL[A]: DecodeResult[A] @?> (String, CursorHistory) =
    PLens(_.result.fold(q => Some(Store(r => DecodeResult.failResult(r._1, r._2), q)),_ => None))

  def failedResultMessageL[A]: DecodeResult[A] @?> String =
    ~Lens.firstLens compose failedResultL[A]

  def failedResultHistoryL[A]: DecodeResult[A] @?> CursorHistory =
    ~Lens.secondLens compose failedResultL[A]

  implicit def DecodeResultMonad: Monad[DecodeResult] with Traverse[DecodeResult] = new Monad[DecodeResult] with Traverse[DecodeResult] {
    def point[A](a: => A) = DecodeResult.ok(a)
    def bind[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
    def traverseImpl[G[_]: Applicative, A, B](fa: DecodeResult[A])(f: A => G[B]): G[DecodeResult[B]] =
      fa.fold(
        (s, h) => DecodeResult.fail[B](s, h).pure[G],
        a => f(a).map(DecodeResult.ok)
      )
  }

  type DecodeEither[A] = Either[(String, CursorHistory), A]

  val decodeResultIsoFunctor: IsoFunctor[DecodeResult, DecodeEither] = new IsoFunctorTemplate[DecodeResult, DecodeEither] {
    def to[A](decodeResult: DecodeResult[A]) = decodeResult.result
    def from[A](either: DecodeEither[A]) = DecodeResult[A](either)
  }

  def decodeResultIsoSet[A]: IsoSet[DecodeResult[A], DecodeEither[A]] = new IsoSet[DecodeResult[A], DecodeEither[A]] {
    def to = decodeResultIsoFunctor.to[A]
    def from = decodeResultIsoFunctor.from[A]
  }

  implicit def DecodeResultEqual[A: Equal]: Equal[DecodeResult[A]] = new IsomorphismEqual[DecodeResult[A], DecodeEither[A]] {
    def G = eitherEqual[(String, CursorHistory), A](implicitly, implicitly)
    def iso = decodeResultIsoSet
  }

  implicit def DecodeResultShow[A : Show]: Show[DecodeResult[A]] = new IsomorphismShow[DecodeResult[A], DecodeEither[A]] {
    def G = new Show[Either[(String, CursorHistory), A]]{
      override def show(e: Either[(String, CursorHistory), A]) = {
        e match {
          case Left(l) => l.show
          case Right(r) => r.show
        }
      }
    }
    def iso = decodeResultIsoSet
  }
}
