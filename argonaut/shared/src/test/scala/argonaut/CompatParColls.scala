package argonaut

// https://github.com/scala/scala-parallel-collections/issues/22
private[argonaut] object CompatParColls {
  val Converters = {
    import Compat._

    {
      import scala.collection.parallel._

      CollectionConverters
    }
  }

  object Compat {
    object CollectionConverters
  }
}
