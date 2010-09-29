package com.ephox.argonaut

/**
 * A data type representing a function from a [[com.ephox.argonaut.Json]] to a [[scala.Option]] of any value.
 * This is related to the [[http://en.wikipedia.org/wiki/Kleisli_category Kleisli Category]] specialised for the Option monad.
 *
 * @author Tony Morris
 * @author Dylan Just
 */
sealed trait JsonOptionK[A] {
  /**
   * A function which is applied to a `Json` value to produce a possible value of an arbitrary type.
   *
   * @param j The `Json` value to apply to.
   */
  def apply(j: Json): Option[A]

  import JsonOptionK._

  /**
   * Lifts the given function into the `JSONOptionK` environment by implementing ''a covariant functor map.''
   *
   * @param f The function to lift.
   */
  def map[B](f: A => B) =
    jsonOptionK(apply(_) map f)

  /**
   * Applies the given function in the `JSONOptionK` environment by implementing ''an applicative functor apply.''
   *
   * @param f The environmental function to apply. 
   */
  def <*>[B](f: JsonOptionK[A => B]) =
    for(a <- this;
        b <- f)
    yield b(a)

  /**
   * Binds the given function in the `JSONOptionK` environment by implementing ''a monadic bind.''
   *
   * @param f The function to bind.
   */
  def flatMap[B](f: A => JsonOptionK[B]) =
    jsonOptionK(j => apply(j) flatMap (f(_)(j)))

  /**
   * Compose two values using the given function.
   *
   * @param k The second value to compose.
   * @param z The function to compose with.
   */
  def compose[B, C](k: JsonOptionK[B], z: (A, B) => C): JsonOptionK[C] =
    jsonOptionK(j => for(a <- apply(j);
                         b <- k(j))
                     yield z(a, b))

  /**
   * Compose two values into a pair.
   *
   * @param k The second value to compose.
   */
  def ---[B](k: JsonOptionK[B]): JsonOptionK[(A, B)] =
    compose(k, (a: A, b: B) => (a, b))

  /**
   * Bind the given value with this `JSONOptionK`.
   *
   * @param j The value to bind.
   */
  def =<<(j: Option[Json]): Option[A] = j flatMap (apply(_))
}

/**
 * Utility functions for `JSONOptionK` values.
 *
 * @author Tony Morris
 * @author Dylan Just
 */
object JsonOptionK {
  /**
   * Construct a `JSONOptionK` value using the given function.
   *
   * @param f The function to construct with.
   */
  def jsonOptionK[A](f: Json => Option[A]): JsonOptionK[A] = new JsonOptionK[A] {
    override def apply(j: Json) = f(j) 
  }

  /**
   * Construct a `JSONOptionK` value that ignores its argument and always returns the given value.
   *
   * @param a The constant value to construct with.
   */
  def constant[A](a: => Option[A]) = jsonOptionK(_ => a)

  /**
   * Construct a `JSONOptionK` value that ignores its argument and always returns `None`.
   */
  def constantNone[A] = constant(None)

  /**
   * Construct a `JSONOptionK` value that ignores its argument and always returns `Some` with the given value.
   *
   * @param a The value to return a `Some` with.
   */
  def constantSome[A](a: => A) = constant(Some(a))

  /**
   * Construct a `JSONOptionK` value that returns the given value in `Some`.
   */
  def unital = jsonOptionK(Some(_))

  /**
   * A `JSONOptionK` that returns a possible `JSON null` value.
   */
  val nulll = jsonOptionK(_.nulll)

  /**
   * A `JSONOptionK` that returns a possible `JSON bool` value.
   */
  val bool = jsonOptionK(_.bool)

  /**
   * A `JSONOptionK` that returns a possible `JSON number` value.
   */
  val number = jsonOptionK(_.number)

  /**
   * A `JSONOptionK` that returns a possible `JSON string` value.
   */
  val string = jsonOptionK(_.string)

  /**
   * A `JSONOptionK` that returns a possible `JSON array` value.
   */
  val array = jsonOptionK(_.array)

  /**
   * A `JSONOptionK` that returns a possible `JSON object` value.
   */
  val objectt = jsonOptionK(_.objectt)

  /**
   * Returns a value that, if produces a `JSON array`, runs the given function on each element through the `Option` monad.
   *
   * @param o The function to run on each `JSON array` element.
   */
  def arrayOf[A](o: JsonOptionK[A]): JsonOptionK[Option[List[A]]] =
    lift2Foldr[Json, A](array)((a, b) =>
      lift2ConsOption(o(a), b))

  /**
   * Returns a value that, if produces a `JSON object`, runs the given function on each element through the `Option` monad.
   *
   * @param o The function to run on each `JSON object` element.
   */
  def objectOf[A](f: String => JsonOptionK[A]): JsonOptionK[Option[List[A]]] =
    lift2Foldr[(String, Json), A](objectt) {
      case ((a, u), b) => lift2ConsOption(f(a)(u), b)
    }

  /**
   * Returns a value that, if produces a `JSON array`, runs the given function on each element through the `Option` monad.
   * A monadic level is reduced.
   *
   * @param o The function to run on each `JSON array` element.
   */
  def joinArrayOf[A](o: JsonOptionK[A]): JsonOptionK[List[A]] =
    jsonOptionK(j => arrayOf(o)(j).flatMap(z => z))

  /**
   * Returns a value that, if produces a `JSON object`, runs the given function on each element through the `Option` monad.
   * A monadic level is reduced.
   *
   * @param o The function to run on each `JSON object` element.
   */
  def joinObjectOf[A](o: String => JsonOptionK[A]): JsonOptionK[List[A]] =
    jsonOptionK(j => objectOf(o)(j).flatMap(z => z)) 

  private def lift2Foldr[A, B](a: JsonOptionK[List[A]])(f: (A, Option[List[B]]) => Option[List[B]]) =
    a.map(_.foldRight[Option[List[B]]](Some(Nil))(f))

  // lifts(2) list.cons into the Option monad
  private def lift2ConsOption[A](a: Option[A], b: Option[List[A]]): Option[List[A]] =
    for(x <- a;
        y <- b)
    yield x :: y
}
