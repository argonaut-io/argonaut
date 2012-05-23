package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

trait Context {
  val toList: List[ContextElement]

  def +:(e: ContextElement): Context =
    Context.build(e :: toList)
}

object Context extends Contexts

trait Contexts {
  private[argonaut] def build(x: List[ContextElement]): Context =
    new Context {
      val toList = x
    }

  def empty: Context =
    new Context {
      val toList = Nil
    }

  implicit val ContextInstances: Equal[Context] with Show[Context] =
    new Equal[Context] with Show[Context] {
      def equal(c1: Context, c2: Context) =
        Equal.equalBy((_: Context).toList).equal(c1, c2)
      def show(c: Context) =
        c.toList.map(_.show).intersperse(List('\\')).join
    }
}

sealed trait ContextElement {
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
  def arrayC(n: Int, j: Json): ContextElement =
    ArrayContext(n, j)

  def objectC(f: JsonField, j: Json): ContextElement =
    ObjectContext(f, j)

  implicit val ContextElementInstances: Equal[ContextElement] with Show[ContextElement] =
    new Equal[ContextElement] with Show[ContextElement] {
      def equal(c1: ContextElement, c2: ContextElement) =
        c1 match {
          case ArrayContext(n1, j1) => c2 match {
            case ArrayContext(n2, j2) => n1 === n2 && j1 === j2
            case ObjectContext(_, _) => false
          }
          case ObjectContext(f1, j1) => c2 match {
            case ObjectContext(f2, j2) => f1 === f2 && j1 === j2
            case ArrayContext(_, _) => false
          }
        }

      def show(c: ContextElement) =
        c match {
          case ArrayContext(n, j) => ("!" + n).toList
          case ObjectContext(f, j) => ("@" + f).toList
        }
    }

}