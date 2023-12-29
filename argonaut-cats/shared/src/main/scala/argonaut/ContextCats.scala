package argonaut

import ContextElementCats._
import JsonCats._
import cats._
import instances.all._
import syntax.eq._
import syntax.show._

object ContextCats extends ContextCatss

trait ContextCatss {
  implicit val ContextInstances: Eq[Context] & Show[Context] = {
    new Eq[Context] with Show[Context] {
      def eqv(c1: Context, c2: Context) = {
        Eq.by((_: Context).toList).eqv(c1, c2)
      }
      override def show(c: Context) = {
        c.toList.map(_.show).mkString(".")
      }
    }
  }
}

object ContextElementCats extends ContextElementCatss

trait ContextElementCatss {
  implicit val ContextElementInstances: Eq[ContextElement] & Show[ContextElement] = {
    new Eq[ContextElement] with Show[ContextElement] {
      override def eqv(c1: ContextElement, c2: ContextElement) = {
        c1 match {
          case ArrayContext(n1, j1) =>
            c2 match {
              case ArrayContext(n2, j2) => n1 === n2 && j1 === j2
              case ObjectContext(_, _) => false
            }
          case ObjectContext(f1, j1) =>
            c2 match {
              case ObjectContext(f2, j2) => f1 === f2 && j1 === j2
              case ArrayContext(_, _) => false
            }
        }
      }

      override def show(c: ContextElement) = {
        c match {
          case ArrayContext(n, j) => "[" + n + "]"
          case ObjectContext(f, j) => "{" + f + "}"
        }
      }
    }
  }
}
