package argonaut

import scalaz._, Scalaz._
import Json._
import ContextElement._

/**
 * Represents a position in a JSON value and allows moving around the JSON value. Also known as a "zipper." The cursor has a focus representing the current position being referred to by the cursor. Users may update the focus using `withFocus` (or the `>->` alias) and move the cursor around with `left`, `right`, `field`, `downArray`, `downField` and `up`.
 *
 * @see Shift
 * @author Tony Morris
 */
sealed trait Cursor {
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
    HCursor(this, Monoid[CursorHistory].zero)

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

  /** Update the focus with the given function in a functor (alias for `withFocusM`). */
  def >-->[F[+_]: Functor](k: Json => F[Json]): F[Cursor] =
    withFocusM(k)

  /** Update the focus with the given function in a functor (alias for `>-->`). */
  def withFocusM[F[+_]: Functor](k: Json => F[Json]): F[Cursor] =
    this match {
      case CJson(j) =>
        Functor[F].map(k(j))(CJson(_))
      case CArray(p, _, l, j, r) =>
        Functor[F].map(k(j))(CArray(p, true, l, _, r))
      case CObject(p, _, x, (f, j)) =>
        Functor[F].map(k(j))(q => CObject(p, true, x, (f, q)))
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
    focus.obj map  (_.fieldSet)

  /**
   * All field names in a JSON object.
   */
  def fields: Option[List[JsonField]] =
    focus.obj map  (_.fields)

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
        val h::t = l.reverse ::: j :: r
        Some(CArray(p, u, Nil, h, t))
      }
      case _ => None
    }

  /** Move the cursor to the last in a JSON array. */
  def last: Option[Cursor] =
    this match {
      case CArray(p, u, l, x, r) => {
        val h::t = r.reverse ::: x :: l
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
        case Some(w) => r(if(p(w.focus)) Some(w) else w.left)
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
        val q = jArray(l.reverse ::: r)
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
        val h::t = l.reverse ::: r
        Some(CArray(p, true, Nil, h, t))
      }
      case _ => None
    }

  /** Deletes the JSON value at focus and moves to the last in a JSON array. */
  def deleteGoLast: Option[Cursor] =
    this match {
      case CArray(p, _, l, _, r) => {
        val h::t = r.reverse ::: l
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
        val q = jArray(l.reverse ::: j :: r)
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
          val q = jArray(l.reverse ::: j :: r)
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

  def traverseBreak[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, Cursor, Option[Cursor]]): Endo[X] =
    Endo(x => {
      @annotation.tailrec
      def spin(z: X, d: Cursor): X = {
        val (q, k) = r run d run z
        k match {
          case None => q
          case Some(a) => spin(q, a)
        }
      }

      spin(x, this)
    })

  def traverse[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, Cursor, Cursor]): Endo[X] =
    traverseBreak(r map (Some(_)))

}
private case class CJson(j: Json) extends Cursor
private case class CArray(p: Cursor, u: Boolean, ls: List[Json], x: Json, rs: List[Json]) extends Cursor
private case class CObject(p: Cursor, u: Boolean, o: JsonObject, x: (JsonField, Json)) extends Cursor

object Cursor extends Cursors {
  def apply(j: Json): Cursor =
    CJson(j)
}

trait Cursors {
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
    def equal(c1: Cursor, c2: Cursor) =
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

    override def show(c: Cursor) =
      c.context.show + " ==> " + c.focus.show
  }
}
