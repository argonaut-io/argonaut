package argonaut

import scalaz._, Scalaz._

sealed trait DecodeResult[+A] {
  val result: (String, CursorHistory) \/ A

  def fold[X](
    failure: (String, CursorHistory) => X,
    value: A => X
  ): X = result.fold({ case (m, h) => failure(m, h) }, value)

  final def loop[X, B >: A](e: (String, CursorHistory) => X, f: B => X \/ DecodeResult[B]): X =
    error("todo")
/*
  @annotation.tailrec
  final def loop[X](e: (String, CursorHistory) => X, f: A => X \/ DecodeResult[A]): X =
    if (isError)
      e(message.get, history.get)
    else
      f(value.get) match {
        case -\/(x) => x
        case \/-(a) => a.loop(e, f)
        }
 */


  def isError: Boolean =
    result.isLeft

  def map[B](f: A => B): DecodeResult[B] =
    DecodeResult.build(result map f)

  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] =
    DecodeResult.build(result flatMap (f(_).result))

  def message: Option[String] =
    failure map (_._1)

  def history: Option[CursorHistory] =
    failure map (_._2)

  def value: Option[A] =
    result.toOption

  def failure: Option[(String, CursorHistory)] =
    result.swap.toOption


  def toEither: Either[(String, CursorHistory), A] =
    result.toEither

  def option: DecodeResult[Option[A]] =
    result.fold(
      { case (s, h) => h.head filter (_.succeeded) match {
        case None => DecodeResult(None)
        case Some(_) => DecodeResult.failedResult(s, h)
      }},
      a => DecodeResult(Some(a))
    )

  def |||[AA >: A](r: => DecodeResult[AA]): DecodeResult[AA] =
    DecodeResult.build(result.fold(_ => r.result, _ => result))
}

object DecodeResult extends DecodeResults {
  def apply[A](a: A): DecodeResult[A] = new DecodeResult[A] {
    val result = a.right
  }
}

trait DecodeResults {
  private[argonaut] def build[A](x: (String, CursorHistory) \/ A): DecodeResult[A] =
    new DecodeResult[A] {
      val result = x
    }

  def failedResult[A](s: String, h: CursorHistory): DecodeResult[A] =
    new DecodeResult[A] {
      val result: (String, CursorHistory) \/ A = (s, h).left
    }

  def failedResultL[A]: DecodeResult[A] @?> (String, CursorHistory) =
    PLens(_.result.fold(q => Some(Store(r => failedResult(r._1, r._2), q)),_ => None))

  def failedResultMessageL[A]: DecodeResult[A] @?> String =
    ~Lens.firstLens compose failedResultL[A]

  def failedResultHistoryL[A]: DecodeResult[A] @?> CursorHistory =
    ~Lens.secondLens compose failedResultL[A]

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def point[A](a: => A) = DecodeResult(a)
    def bind[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }

}
