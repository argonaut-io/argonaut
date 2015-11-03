package argonaut

import Json._

sealed abstract class Context {
  val toList: List[ContextElement]

  def +:(e: ContextElement): Context =
    Context.build(e :: toList)
}

object Context extends Contexts {
  def empty: Context =
    new Context {
      val toList = Nil
    }
}

trait Contexts {
  private[argonaut] def build(x: List[ContextElement]): Context =
    new Context {
      val toList = x
    }
}

sealed abstract class ContextElement extends Product with Serializable {
  def json: Json =
    this match {
      case ArrayContext(_, j) => j
      case ObjectContext(_, j) => j
    }

  def field: Option[JsonField] =
    this match {
      case ArrayContext(_, _) => None
      case ObjectContext(f, _) => Some(f)
    }

  def index: Option[Int] =
    this match {
      case ArrayContext(n, _) => Some(n)
      case ObjectContext(_, _) => None
    }
}
private case class ArrayContext(n: Int, j: Json) extends ContextElement
private case class ObjectContext(f: JsonField, j: Json) extends ContextElement

object ContextElement extends ContextElements

trait ContextElements {
  def arrayContext(n: Int, j: Json): ContextElement =
    ArrayContext(n, j)

  def objectContext(f: JsonField, j: Json): ContextElement =
    ObjectContext(f, j)
}
