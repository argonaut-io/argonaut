package argonaut

import Json._
import scalaz._
import std.anyVal._, std.list._, std.string._
import syntax.show._, syntax.equal._
import syntax.std.list._
import JsonScalaz._
import ContextElementScalaz._

object ContextScalaz extends ContextScalazs

trait ContextScalazs {
  implicit val ContextInstances: Equal[Context] with Show[Context] = {
    new Equal[Context] with Show[Context] {
      def equal(c1: Context, c2: Context) = {
        Equal.equalBy((_: Context).toList).equal(c1, c2)
      }
      override def show(c: Context) = {
        c.toList.map(_.shows).intersperse(".").mkString
      }
    }
  }
}

object ContextElementScalaz extends ContextElementScalazs

trait ContextElementScalazs {
  implicit val ContextElementInstances: Equal[ContextElement] with Show[ContextElement] = {
    new Equal[ContextElement] with Show[ContextElement] {
      override def equal(c1: ContextElement, c2: ContextElement) = {
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
