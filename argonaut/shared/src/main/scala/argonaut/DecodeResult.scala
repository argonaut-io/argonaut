package argonaut

case class DecodeResult[A](result: Either[(String, CursorHistory), A]) {
  def fold[X](
    failure: (String, CursorHistory) => X,
    value: A => X
  ): X = result.fold({case (m, h) => failure(m, h)}, value)

  final def loop[X, B](e: (String, CursorHistory) => X, f: B => Either[X, DecodeResult[B]])(implicit ev: A <:< B): X =
    DecodeResult.loop(this.map(ev), e, f)

  def isError: Boolean =
    result.isLeft

  def map[B](f: A => B): DecodeResult[B] =
    DecodeResult(result.right.map(f))

  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] =
    DecodeResult(result.right.flatMap(f(_).result))

  def message: Option[String] =
    failure.map(_._1)

  def history: Option[CursorHistory] =
    failure.map(_._2)

  def toOption: Option[A] =
    result.right.toOption

  def toEither: Either[(String, CursorHistory), A] =
    result

  def getOr[AA >: A](els: => AA): AA =
    toOption.getOrElse(els)

  /** alias for `toOption` */
  def value: Option[A] =
    result.right.toOption

  def failure: Option[(String, CursorHistory)] =
    result.left.toOption

  def option: DecodeResult[Option[A]] =
    result match {
      case Left((s, h)) => DecodeResult.fail(s, h)
      case Right(a) => DecodeResult.ok(Some(a))
    }

  def |||[AA >: A](r: => DecodeResult[AA]): DecodeResult[AA] =
    DecodeResult(result.fold(_ => r.result, _ => result))

  override def toString(): String = s"DecodeResult(${result})"
}

object DecodeResult extends DecodeResults {
  def ok[A](value: A): DecodeResult[A] =
    DecodeResult(Right(value))

  def fail[A](s: String, h: CursorHistory): DecodeResult[A] =
    DecodeResult(Left((s, h)))
}

trait DecodeResults {
  type DecodeEither[A] = Either[(String, CursorHistory), A]

  def okResult[A](value: A): DecodeResult[A] =
    DecodeResult.ok(value)

  def failResult[A](s: String, h: CursorHistory): DecodeResult[A] =
    DecodeResult.fail(s, h)

  @annotation.tailrec
  final def loop[A, X](d: DecodeResult[A], e: (String, CursorHistory) => X, f: A => Either[X, DecodeResult[A]]): X = {
    if (d.isError) {
      e(d.message.get, d.history.get)
    } else {
      f(d.value.get) match {
        case Left(x) => x
        case Right(a) => loop(a, e, f)
      }
    }
  }
}
