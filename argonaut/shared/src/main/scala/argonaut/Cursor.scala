package argonaut

import Json._
import ContextElement._

/**
 * Represents a position in a JSON value and allows moving around the JSON value. Also known as a "zipper." The cursor has a focus representing the current position being referred to by the cursor. Users may update the focus using `withFocus` (or the `>->` alias) and move the cursor around with `left`, `right`, `field`, `downArray`, `downField` and `up`.
 *
 * @author Tony Morris
 */
sealed abstract class Cursor extends Product with Serializable {
  /** Return the current context of the focus. */
  def context: Context =
    this match {
      case CJson(_) => Context.empty
      case CArray(p, _, l, j, _) => arrayContext(l.length, j) +: p.context
      case CObject(p, _, _, (f, j)) => objectContext(f, j) +: p.context
    }

  /**
   * A HCursor for this cursor that tracks history.
   */
  def hcursor: HCursor =
    HCursor(this, CursorHistory.empty)

  /**
   * An ACursor for this cursor that tracks history.
   */
  def acursor: ACursor =
    hcursor.acursor

  /** Return the current focus. */
  def focus: Json =
    this match {
      case CJson(j) => j
      case CArray(_, _, _, j, _) => j
      case CObject(_, _, _, (_, j)) => j
    }

  /** Update the focus with the given function (alias for `withFocus`). */
  def >->(k: Json => Json): Cursor =
    withFocus(k)

  /** Update the focus with the given function (alias for `>->`). */
  def withFocus(k: Json => Json): Cursor =
    this match {
      case CJson(j) =>
        CJson(k(j))
      case CArray(p, _, l, j, r) =>
        CArray(p, true, l, k(j), r)
      case CObject(p, _, x, (f, j)) =>
        CObject(p, true, x, (f, k(j)))
    }

  /** Set the focus to the given value (alias for `:=`). */
  def set(j: Json): Cursor =
    withFocus(_ => j)

  /** Set the focus to the given value (alias for `set`). */
  def :=(j: Json): Cursor =
    set(j)

  /**
   * Return the values left of focus in a JSON array.
   */
  def lefts: Option[JsonArray] =
    this match {
      case CArray(_, _, l, _, _) => Some(l)
      case _ => None
    }

  /**
   * Return the values right of focus in a JSON array.
   */
  def rights: Option[JsonArray] =
    this match {
      case CArray(_, _, _, _, r) => Some(r)
      case _ => None
    }

  /**
   * All field names in a JSON object.
   */
  def fieldSet: Option[Set[JsonField]] =
    focus.obj.map(_.fieldSet)

  /**
   * All field names in a JSON object.
   */
  def fields: Option[List[JsonField]] =
    focus.obj.map(_.fields)

  /** Move the cursor left in a JSON array. */
  def left: Option[Cursor] =
    this match {
      case CArray(p, u, l, j, r) => l match {
        case Nil => None
        case h::t => Some(CArray(p, u, t, h, j::r))
      }
      case _ => None
    }

  /** Move the cursor right in a JSON array. */
  def right: Option[Cursor] =
    this match {
      case CArray(p, u, l, j, r) => r match {
        case Nil => None
        case h::t => Some(CArray(p, u, j::l, h, t))
      }
      case _ => None
    }

  /** Move the cursor to the first in a JSON array. */
  def first: Option[Cursor] =
    this match {
      case CArray(p, u, l, j, r) => {
        val h::t = l reverse_::: j :: r
        Some(CArray(p, u, Nil, h, t))
      }
      case _ => None
    }

  /** Move the cursor to the last in a JSON array. */
  def last: Option[Cursor] =
    this match {
      case CArray(p, u, l, x, r) => {
        val h::t = r reverse_::: x :: l
        Some(CArray(p, u, t, h, Nil))
      }
      case _ => None
    }

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `leftN`). */
  def -<-:(n: Int): Option[Cursor] =
    leftN(n)

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `-<-:`). */
  def leftN(n: Int): Option[Cursor] =
    if(n < 0)
      :->-(-n)
    else {
      @annotation.tailrec
      def r(x: Int, c: Option[Cursor]): Option[Cursor] =
        if (x == 0)
          c
        else
          r(x - 1, c flatMap (_.left))
      r(n, Some(this))
    }

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `rightN`). */
  def :->-(n: Int): Option[Cursor] =
    rightN(n)

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `:->-`). */
  def rightN(n: Int): Option[Cursor] =
    if(n < 0)
      -<-:(-n)
    else {
      @annotation.tailrec
      def r(x: Int, c: Option[Cursor]): Option[Cursor] =
        if (x == 0)
          c
        else
          r(x - 1, c flatMap (_.right))
      r(n, Some(this))
    }

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `leftAt`). */
  def ?<-:(p: Json => Boolean): Option[Cursor] =
    leftAt(p)

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `?<-:`). */
  def leftAt(p: Json => Boolean): Option[Cursor] = {
    @annotation.tailrec
    def r(c: Option[Cursor]): Option[Cursor] =
      c match {
        case None => None
        case Some(w) => if (p(w.focus)) Some(w) else r(w.left)
      }

    r(left)
  }

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `rightAt`). */
  def :->?(p: Json => Boolean): Option[Cursor] =
    rightAt(p)

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `:->?`). */
  def rightAt(p: Json => Boolean): Option[Cursor] =
    right.flatMap(_.find(p))

  /** Find the first element at or to the right of focus in a JSON array where the given predicate matches the focus. */
  def find(p: Json => Boolean): Option[Cursor] = {
    @annotation.tailrec
    def r(c: Option[Cursor]): Option[Cursor] =
      c match {
        case None => None
        case Some(w) => if (p(w.focus)) Some(w) else r(w.right)
      }

    r(Some(this))
  }

  /** Move the cursor to the given sibling field in a JSON object (alias for `field`). */
  def --(q: JsonField): Option[Cursor] =
    field(q)

  /** Move the cursor to the given sibling field in a JSON object (alias for `--`). */
  def field(q: JsonField): Option[Cursor] =
    this match {
      case CObject(p, u, o, (f, j)) =>
        o(q) map (jj => CObject(p, u, o, (q, jj)))
      case _ => None
    }

  /** Move the cursor down to a JSON object at the given field (alias for `downField`). */
  def --\(q: JsonField): Option[Cursor] =
    downField(q)

  /** Move the cursor down to a JSON object at the given field (alias for `--\`). */
  def downField(q: JsonField): Option[Cursor] =
    focus.obj flatMap (o => o(q) map (jj => CObject(this, false, o, (q, jj))))

  /** Move the cursor down to a JSON array at the first element (alias for `\\`). */
  def downArray: Option[Cursor] =
    focus.array flatMap (_ match {
      case Nil => None
      case h::t => Some(CArray(this, false, Nil, h, t))
    })

  /** Move the cursor down to a JSON array at the first element (alias for `downArray`). */
  def \\ : Option[Cursor] =
    downArray

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `downAt`). */
  def -\(p: Json => Boolean): Option[Cursor] =
    downAt(p)

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `-\`). */
  def downAt(p: Json => Boolean): Option[Cursor] =
    downArray flatMap (_ find p)

  /** Move the cursor down to a JSON array at the given index (alias for `downN`). */
  def =\(n: Int): Option[Cursor] =
    downN(n)

  /** Move the cursor down to a JSON array at the given index (alias for `=\`). */
  def downN(n: Int): Option[Cursor] =
    downArray flatMap (_ :->- n)

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def delete : Option[Cursor] =
    deleteGoParent

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def unary_! : Option[Cursor] =
    deleteGoParent

  /** Deletes the JSON value at focus and moves up to parent (alias for `unary_!`). */
  def deleteGoParent: Option[Cursor] =
    this match {
      case CJson(_) =>
        None
      case CArray(p, _, l, _, r) => {
        val q = jArray(l reverse_::: r)
        Some(p match {
          case CJson(_) =>
            CJson(q)
          case CArray(pp, _, l1, _, r1) =>
            CArray(pp, true, l1, q, r1)
          case CObject(pp, _, oo, (ff, _)) =>
            CObject(pp, true, oo, (ff, q))
        })
      }
      case CObject(p, _, o, (f, _)) => {
        val q = jObject(o - f)
        Some(p match {
          case CJson(_) =>
            CJson(q)
          case CArray(pp, _, l1, _, r1) =>
            CArray(pp, true, l1, q, r1)
          case CObject(pp, _, oo, (ff, _)) =>
            CObject(pp, true, oo, (ff, q))
        })
      }
    }

  /** Deletes the JSON value at focus and moves to the left in a JSON array. */
  def deleteGoLeft: Option[Cursor] =
    this match {
      case CArray(p, _, l, _, r) => l match {
        case Nil => None
        case h::t => Some(CArray(p, true, t, h, r))
      }
      case _ => None
    }

  /** Deletes the JSON value at focus and moves to the right in a JSON array. */
  def deleteGoRight: Option[Cursor] =
    this match {
      case CArray(p, _, l, _, r) => r match {
        case Nil => None
        case h::t => Some(CArray(p, true, l, h, t))
      }
      case _ => None
    }

  /** Deletes the JSON value at focus and moves to the first in a JSON array. */
  def deleteGoFirst: Option[Cursor] =
    this match {
      case CArray(p, _, l, _, r) => {
        val h::t = l reverse_::: r
        Some(CArray(p, true, Nil, h, t))
      }
      case _ => None
    }

  /** Deletes the JSON value at focus and moves to the last in a JSON array. */
  def deleteGoLast: Option[Cursor] =
    this match {
      case CArray(p, _, l, _, r) => {
        val h::t = r reverse_::: l
        Some(CArray(p, true, t, h, Nil))
      }
      case _ => None
    }

  /** Deletes the JSON value at focus and moves to the given sibling field in a JSON object. */
  def deleteGoField(q: JsonField): Option[Cursor] =
    this match {
      case CObject(p, _, o, (f, _)) =>
        o(q) map (jj => CObject(p, true, o - f, (q, jj)))
      case _ => None
    }

  /** Deletes all JSON values to left of focus in a JSON array. */
  def deleteLefts: Option[Cursor] =
    this match {
      case CArray(p, _, _, j, r) =>
        Some(CArray(p, true, Nil, j, r))
      case _ =>
        None
    }

  /** Deletes all JSON values to right of focus in a JSON array. */
  def deleteRights: Option[Cursor] =
    this match {
      case CArray(p, _, l, j, _) =>
        Some(CArray(p, true, l, j, Nil))
      case _ =>
        None
    }

  /** Set the values to the left of focus in a JSON array. */
  def setLefts(x: List[Json]): Option[Cursor] =
    this match {
      case CArray(p, _, _, j, r) =>
        Some(CArray(p, true, x, j, r))
      case _ =>
        None
    }

  /** Set the values to the right of focus in a JSON array. */
  def setRights(x: List[Json]): Option[Cursor] =
    this match {
      case CArray(p, _, l, j, _) =>
        Some(CArray(p, true, l, j, x))
      case _ =>
        None
    }

  /** Move the cursor up one step to the parent context. */
  def up: Option[Cursor] =
    this match {
      case CJson(_) =>
        None
      case CArray(p, u, l, j, r) => {
        val q = jArray(l reverse_::: j :: r)
        Some(p match {
          case CJson(_) =>
            CJson(q)
          case CArray(pp, v, l1, _, r1) =>
            CArray(pp, u || v, l1, q, r1)
          case CObject(pp, v, oo, (ff, _)) =>
            CObject(pp, u || v, if(u) oo + (ff, q) else oo, (ff, q))
        })
      }
      case CObject(p, u, o, (f, j)) => {
        val q = jObject(if(u) o + (f, j) else o)
        Some(p match {
          case CJson(_) =>
            CJson(q)
          case CArray(pp, v, l1, _, r1) =>
            CArray(pp, u || v, l1, q, r1)
          case CObject(pp, v, oo, (ff, _)) =>
            CObject(pp, u || v, oo, (ff, q))
        })
      }
    }

  /** Unapplies the cursor to the top-level parent (alias for `undo`). */
  def unary_- : Json =
    undo

  /** Unapplies the cursor to the top-level parent (alias for `unary_-`). */
  def undo: Json = {
    @annotation.tailrec
    def goup(c: Cursor): Json =
      c match {
        case CJson(j) =>
          j
        case CArray(p, u, l, j, r) => {
          val q = jArray(l reverse_::: j :: r)
          goup(p match {
              case CJson(_) =>
                CJson(q)
              case CArray(pp, v, l1, _, r1) =>
                CArray(pp, u || v, l1, q, r1)
              case CObject(pp, v, oo, (ff, _)) =>
                CObject(pp, u || v, if(u) oo + (ff, q) else oo, (ff, q))
            })
        }
        case CObject(p, u, o, (f, j)) => {
          val q = jObject(if(u) o + (f, j) else o)
          goup(p match {
            case CJson(_) =>
              CJson(q)
            case CArray(pp, v, l1, _, r1) =>
              CArray(pp, u || v, l1, q, r1)
            case CObject(pp, v, oo, (ff, _)) =>
              CObject(pp, u || v, oo, (ff, q))
          })
        }
      }
    goup(this)
  }
}

private case class CJson(j: Json) extends Cursor
private case class CArray(p: Cursor, u: Boolean, ls: List[Json], x: Json, rs: List[Json]) extends Cursor {
  override def equals(other: Any): Boolean = {
    other match {
      case CArray(p2, _, ls2, x2, rs2) => p == p2 && ls == ls2 && x == x2 && rs == rs2
      case _ => false
    }
  }
}
private case class CObject(p: Cursor, u: Boolean, o: JsonObject, x: (JsonField, Json)) extends Cursor {
  override def equals(other: Any): Boolean = {
    other match {
      case CObject(p2, _, o2, x2) => p == p2 && o == o2 && x == x2
      case _ => false
    }
  }
}

object Cursor extends Cursors {
  def apply(j: Json): Cursor = CJson(j)
}

trait Cursors {
}
