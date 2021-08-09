package argonaut

/** A typeclass for encode an arbitrary value as a JSON key.
 *
 * @example {{{
 * final case class Foo(value: String)
 * object Foo {
 *   implicit val instance: EncodeJsonKey[Foo] =
 *     EncodeJsonKey.from(_.value)
 * }
 *
 * EncodeJson.of[Map[Foo, Int]]
 * }}}
 */
trait EncodeJsonKey[A] { self =>

  def toJsonKey(key: A): String

  final def contramap[B](f: B => A): EncodeJsonKey[B] = {
    (key: B) => self.toJsonKey(f(key))
  }
}

object EncodeJsonKey {

  @inline def apply[A](implicit A: EncodeJsonKey[A]): EncodeJsonKey[A] = A

  def from[A](f: A => String): EncodeJsonKey[A] = {
    (key: A) => f(key)
  }

  implicit val StringEncodeJsonKey: EncodeJsonKey[String] = from(x => x)
}
