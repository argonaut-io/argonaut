package argonaut

import shapeless.ops.product.ToTuple

object TestCompat {
  implicit class AsTupleOps[A](private val value: A) extends AnyVal {
    def asTuple[B](implicit toTuple: ToTuple.Aux[A, B]): Option[B] =
      Some(toTuple.apply(value))
  }
}
