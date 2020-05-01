package argonaut

import scalaz._, syntax.show._, syntax.equal._
import std.string._, std.list._
import Json._
import JsonScalaz._
import JsonObjectScalaz._
import ContextScalaz._

object CursorScalaz extends CursorScalazs {
}

trait CursorScalazs {
  /**
   * A lens of a cursor's focus.
   */
  val focusL: Cursor @> Json =
    Lens {
      case CJson(j) =>
        Store(CJson(_), j)
      case CArray(p, _, l, j, r) =>
        Store(CArray(p, true, l, _, r), j)
      case CObject(p, _, x, (f, j)) =>
        Store(jj => CObject(p, true, x, (f, jj)), j)
    }

  /**
   * A partial lens of the lefts of a cursor at a JSON array.
   */
  val leftsL: Cursor @?> JsonArray =
    PLens {
      case CArray(p, _, l, j, r) =>
        Some(Store(CArray(p, true, _, j, r), l))
      case _ => None
    }

  /**
   * A partial lens of the left of a cursor at a JSON array.
   */
  val leftL: Cursor @?> Json =
    PLens {
      case CArray(p, _, l, j, r) =>
        l match {
          case Nil => None
          case h::t => Some(Store(q => CArray(p, true, q::t, j, r), h))
        }
      case _ => None
    }

  /**
   * A partial lens of the rights of a cursor at a JSON array.
   */
  val rightsL: Cursor @?> JsonArray =
    PLens {
      case CArray(p, _, l, j, r) =>
        Some(Store(CArray(p, true, _, j, r), l))
      case _ => None
    }

  /**
   * A partial lens of the right of a cursor at a JSON array.
   */
  val rightL: Cursor @?> Json =
    PLens {
      case CArray(p, _, l, j, r) =>
        l match {
          case Nil => None
          case h::t => Some(Store(q => CArray(p, true, q::t, j, r), h))
        }
      case _ => None
    }

  implicit val CursorInstances: Equal[Cursor] with Show[Cursor] = new Equal[Cursor] with Show[Cursor] {
    def equal(c1: Cursor, c2: Cursor) = {
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

    override def show(c: Cursor): Cord = {
      Cord(z"${c.context} ==> ${c.focus}")
    }
  }
}
