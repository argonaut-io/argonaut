package com.ephox.argonaut

sealed trait JsonOptionK[A] {
  def apply(j: Json): Option[A]

  import JsonOptionK._

  def map[B](f: A => B) =
    jsonOptionK(apply(_) map f)

  def <*>[B](f: JsonOptionK[A => B]) =
    for(a <- this;
        b <- f)
    yield b(a)

  def flatMap[B](f: A => JsonOptionK[B]) =
    jsonOptionK(j => apply(j) flatMap (f(_)(j)))

  def compose[B, C](k: JsonOptionK[B], z: (A, B) => C): JsonOptionK[C] =
    jsonOptionK(j => for(a <- apply(j);
                         b <- k(j))
                     yield z(a, b))

  def =<<(j: Option[Json]): Option[A] = j flatMap (apply(_))
}

object JsonOptionK {
  def jsonOptionK[A](f: Json => Option[A]): JsonOptionK[A] = new JsonOptionK[A] {
    override def apply(j: Json) = f(j) 
  }

  def constant[A](a: => Option[A]) = jsonOptionK(_ => a)
  def constantNone[A] = constant(None)
  def constantSome[A](a: => A) = constant(Some(a))
  def arr = jsonOptionK(Some(_))

  val nulll = jsonOptionK(_.nulll)
  val bool = jsonOptionK(_.bool)
  val number = jsonOptionK(_.number)
  val string = jsonOptionK(_.string)
  val array = jsonOptionK(_.array)

  val objectt = jsonOptionK(_.objectt)

  def arrayOf[A](o: JsonOptionK[A]): JsonOptionK[Option[List[A]]] =
    array map (_.foldRight[Option[List[A]]](Some(Nil))((a, b) =>
      for(y <- o(a);
          z <- b)
      yield y :: z))

  def objectOf[A](f: String => JsonOptionK[A]): JsonOptionK[Option[List[A]]] =
    objectt map (_.foldRight[Option[List[A]]](Some(Nil)) { case ((a, u), b) =>
      for(y <- f(a)(u);
          z <- b)
      yield y :: z })
}
