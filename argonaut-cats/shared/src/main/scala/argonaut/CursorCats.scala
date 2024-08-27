package argonaut

import ContextCats.*
import JsonCats.*
import JsonObjectCats.*
import cats.*
import instances.list.*
import instances.string.*
import syntax.eq.*
import syntax.show.*

object CursorCats extends CursorCatss {}

trait CursorCatss {
  implicit val CursorInstances: Eq[Cursor] & Show[Cursor] = new Eq[Cursor] with Show[Cursor] {
    def eqv(c1: Cursor, c2: Cursor) = {
      c1 match {
        case CJson(j1) =>
          c2 match {
            case CJson(j2) => j1 === j2
            case _ => false
          }
        case CArray(p1, _, l1, j1, r1) =>
          c2 match {
            case CArray(p2, _, l2, j2, r2) => p1 === p2 && l1 === l2 && j1 === j2 && r1 === r2
            case _ => false
          }
        case CObject(p1, _, x1, (f1, j1)) =>
          c2 match {
            case CObject(p2, _, x2, (f2, j2)) => p1 === p2 && x1 === x2 && f1 === f2 && j1 === j2
            case _ => false
          }
      }
    }

    override def show(c: Cursor) = {
      c.context.show + " ==> " + c.focus.show
    }
  }
}
