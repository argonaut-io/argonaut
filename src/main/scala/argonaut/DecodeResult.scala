package argonaut

import scalaz._, Isomorphism._
import syntax.either._, std.string._, std.tuple._

case class DecodeResult[A](result:  (String, CursorHistory) \/ A) {
  def fold[X](
    failure: (String, CursorHistory) => X,
    value: A => X
  ): X = result.fold({ case (m, h) => failure(m, h) }, value)

  final def loop[X](e: (String, CursorHistory) => X, f: A => X \/ DecodeResult[A]): X =
    DecodeResult.loop(this, e, f)

  def isError: Boolean =
    result.isLeft

  def map[B](f: A => B): DecodeResult[B] =
    DecodeResult(result map f)

  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] =
    DecodeResult(result flatMap (f(_).result))

  def message: Option[String] =
    failure map (_._1)

  def history: Option[CursorHistory] =
    failure map (_._2)

  def toOption: Option[A] =
    result.toOption

  def toDisjunction: (String, CursorHistory) \/ A =
    result

  def toEither: Either[(String, CursorHistory), A] =
    result.toEither

  def getOr[AA >: A](els: => AA): AA =
    toOption.getOrElse(els)

  /** alias for `toOption` */
  def value: Option[A] =
    result.toOption

  def failure: Option[(String, CursorHistory)] =
    result.swap.toOption

  def option: DecodeResult[Option[A]] =
    result.fold(
      { case (s, h) => h.head filter (_.succeeded) match {
        case None => DecodeResult.ok(None)
        case Some(_) => DecodeResult.fail(s, h)
      }},
      a => DecodeResult.ok(Some(a))
    )

  def |||[AA >: A](r: => DecodeResult[AA]): DecodeResult[AA] =
    DecodeResult(result.fold(_ => r.result, _ => result))

  override def toString(): String = "DecodeResult(%s)".format(result)
}

object DecodeResult extends DecodeResults {
  def ok[A](value: A): DecodeResult[A] =
    DecodeResult(value.right)

  def fail[A](s: String, h: CursorHistory): DecodeResult[A] =
    DecodeResult((s, h).left)
}

trait DecodeResults {
  def okResult[A](value: A): DecodeResult[A] =
    DecodeResult.ok(value)

  def failResult[A](s: String, h: CursorHistory): DecodeResult[A] =
    DecodeResult.fail(s, h)

  @annotation.tailrec
  final def loop[A, X](d: DecodeResult[A], e: (String, CursorHistory) => X, f: A => X \/ DecodeResult[A]): X =
    if (d.isError)
      e(d.message.get, d.history.get)
    else
      f(d.value.get) match {
        case -\/(x) => x
        case \/-(a) => loop(a, e, f)
        }

  def failedResultL[A]: DecodeResult[A] @?> (String, CursorHistory) =
    PLens(_.result.fold(q => Some(Store(r => failResult(r._1, r._2), q)),_ => None))

  def failedResultMessageL[A]: DecodeResult[A] @?> String =
    ~Lens.firstLens compose failedResultL[A]

  def failedResultHistoryL[A]: DecodeResult[A] @?> CursorHistory =
    ~Lens.secondLens compose failedResultL[A]

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def point[A](a: => A) = DecodeResult.ok(a)
    def bind[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }

  type DecodeEither[A] = (String, CursorHistory) \/ A

  val decodeResultIsoFunctor: IsoFunctor[DecodeResult, DecodeEither] = new IsoFunctorTemplate[DecodeResult, DecodeEither] {
    def to[A](decodeResult: DecodeResult[A]) = decodeResult.result
    def from[A](either: DecodeEither[A]) = DecodeResult[A](either)
  }

  def decodeResultIsoSet[A]: IsoSet[DecodeResult[A], DecodeEither[A]] = new IsoSet[DecodeResult[A], DecodeEither[A]] {
    def to = decodeResultIsoFunctor.to[A]
    def from = decodeResultIsoFunctor.from[A]
  }

  implicit def DecodeResultEqual[A: Equal]: Equal[DecodeResult[A]] = new IsomorphismEqual[DecodeResult[A], DecodeEither[A]] {
    def G = \/.DisjunctionEqual(implicitly, implicitly)
    def iso = decodeResultIsoSet
  }

  implicit def DecodeResultShow[A : Show]: Show[DecodeResult[A]] = new IsomorphismShow[DecodeResult[A], DecodeEither[A]] {
    def G = \/.DisjunctionShow(implicitly, implicitly)
    def iso = decodeResultIsoSet
  }
}
