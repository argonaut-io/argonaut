package argonaut

import scalaz._, Scalaz._

sealed trait DecodeResult[+A] {
  val result: Either[(String, CursorHistory), A]

  def map[B](f: A => B): DecodeResult[B] =
    DecodeResult.build(result.right map f)

  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] =
    DecodeResult.build(result.right flatMap (f(_).result))

  def message: Option[String] =
    result.left.toOption map (_._1)

  def history: Option[CursorHistory] =
    result.left.toOption map (_._2)

  def value: Option[A] =
    result.right.toOption

  def option: DecodeResult[Option[A]] =
    result match {
      case Left((s, h)) => h.head filter (_.succeeded) match {
        case None => DecodeResult(None)
        case Some(o) => DecodeResult.failedResult(s, h)
      }
      case Right(a) => DecodeResult(Some(a))
    }


  def |||[AA >: A](r: => DecodeResult[AA]): DecodeResult[AA] =
    DecodeResult.build(result match {
      case Left(_) => r.result
      case Right(_) => result
    })
}

object DecodeResult extends DecodeResults {
  def apply[A](a: A): DecodeResult[A] = new DecodeResult[A] {
    val result = Right(a)
  }
}

trait DecodeResults {
  private[argonaut] def build[A](x: Either[(String, CursorHistory), A]): DecodeResult[A] =
    new DecodeResult[A] {
      val result = x
    }

  def failedResult[A](s: String, h: CursorHistory): DecodeResult[A] =
    new DecodeResult[A] {
      val result: Either[(String, CursorHistory), A] = Left(s, h)
    }

  def failedResultL[A]: DecodeResult[A] @?> (String, CursorHistory) =
    PLens(_.result match {
      case Left(q) => Some(Store(r => failedResult(r._1, r._2), q))
      case Right(_) => None
    })

  def failedResultMessageL[A]: DecodeResult[A] @?> String =
    ~LensT.firstLens compose failedResultL[A]

  def failedResultHistoryL[A]: DecodeResult[A] @?> CursorHistory =
    ~LensT.secondLens compose failedResultL[A]

  implicit def DecodeResultMonad: Monad[DecodeResult] = new Monad[DecodeResult] {
    def point[A](a: => A) = DecodeResult(a)
    def bind[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
    override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f
  }

}
