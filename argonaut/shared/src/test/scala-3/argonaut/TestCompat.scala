package argonaut

import scala.deriving.Mirror.ProductOf

object TestCompat {
  extension [A <: scala.Product](value: A) {
    def asTuple(using mirror: ProductOf[A]): Option[mirror.MirroredElemTypes] =
      Some(Tuple.fromProductTyped(value))
  }
}
