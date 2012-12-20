package argonaut

import scalaz._, Scalaz._

sealed trait HCursor {
  val cursor: Cursor
  val history: CursorHistory

  import Json._

  def acursor: ACursor =
    ACursor(this)

  /**
   * Attempts to decode this cursor focus value to another data type.
   */
  def jdecode[A](implicit e: DecodeJson[A]): DecodeResult[A] =
    e(this)

  /**
   * Attempts to move down onto a field `name` and decode the focus.
   */
  def get[A](name: String)(implicit e: DecodeJson[A]): DecodeResult[A] =
    (this --\ name).jdecode[A]

  def failedACursor: ACursor =
    ACursor.failedACursor(this)

  /** Return the current focus. */
  def focus: Json =
    cursor.focus

  /** Update the focus with the given function (alias for `withFocus`). */
  def >->(k: Json => Json): HCursor =
    withFocus(k)

  /** Update the focus with the given function (alias for `>->`). */
  def withFocus(k: Json => Json): HCursor =
    HCursor(cursor.withFocus(k), history)

  /** Update the focus with the given function in a functor (alias for `withFocusM`). */
  def >-->[F[+_]: Functor](k: Json => F[Json]): F[HCursor] =
    withFocusM(k)

  /** Update the focus with the given function in a functor (alias for `>-->`). */
  def withFocusM[F[+_]: Functor](k: Json => F[Json]): F[HCursor] =
    Functor[F].map(cursor.withFocusM(k))(c => HCursor(c, history))

  /** Set the focus to the given value (alias for `:=`). */
  def set(j: Json): HCursor =
    withFocus(_ => j)

  /** Set the focus to the given value (alias for `set`). */
  def :=(j: Json): HCursor =
    set(j)

  /**
   * Return the values left of focus in a JSON array.
   */
  def lefts: Option[JsonArray] =
    cursor.lefts

  /**
   * Return the values right of focus in a JSON array.
   */
  def rights: Option[JsonArray] =
    cursor.rights

  /**
   * All field names in a JSON object.
   */
  def fieldSet: Option[Set[JsonField]] =
    cursor.fieldSet

  /**
   * All field names in a JSON object.
   */
  def fields: Option[List[JsonField]] =
    cursor.fields

  /** Move the cursor left in a JSON array. */
  def left: ACursor =
    history.acursorElement(Store(_.left, cursor), CursorOpLeft)

  /** Move the cursor right in a JSON array. */
  def right: ACursor =
    history.acursorElement(Store(_.right, cursor), CursorOpRight)

  /** Move the cursor to the first in a JSON array. */
  def first: ACursor =
    history.acursorElement(Store(_.left, cursor), CursorOpFirst)

  /** Move the cursor to the last in a JSON array. */
  def last: ACursor =
    history.acursorElement(Store(_.left, cursor), CursorOpLast)

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `leftN`). */
  def -<-:(n: Int): ACursor =
    leftN(n)

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `-<-:`). */
  def leftN(n: Int): ACursor =
    history.acursorElement(Store(_.leftN(n), cursor), CursorOpLeftN(n))

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `rightN`). */
  def :->-(n: Int): ACursor =
    rightN(n)

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `:->-`). */
  def rightN(n: Int): ACursor =
    history.acursorElement(Store(_.rightN(n), cursor), CursorOpRightN(n))

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `leftAt`). */
  def ?<-:(p: Json => Boolean): ACursor =
    leftAt(p)

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `?<-:`). */
  def leftAt(p: Json => Boolean): ACursor =
    history.acursorElement(Store(_.leftAt(p), cursor), CursorOpLeftAt(p))

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `rightAt`). */
  def :->?(p: Json => Boolean): ACursor =
    rightAt(p)

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `:->?`). */
  def rightAt(p: Json => Boolean): ACursor =
    history.acursorElement(Store(_.rightAt(p), cursor), CursorOpRightAt(p))

  /** Move the cursor to the given sibling field in a JSON object (alias for `field`). */
  def --(q: JsonField): ACursor =
    field(q)

  /** Move the cursor to the given sibling field in a JSON object (alias for `--`). */
  def field(q: JsonField): ACursor =
    history.acursorElement(Store(_.field(q), cursor), CursorOpField(q))

  /** Move the cursor down to a JSON object at the given field (alias for `downField`). */
  def --\(q: JsonField): ACursor =
    downField(q)

  /** Move the cursor down to a JSON object at the given field (alias for `--\`). */
  def downField(q: JsonField): ACursor =
    history.acursorElement(Store(_.downField(q), cursor), CursorOpDownField(q))

  /** Move the cursor down to a JSON array at the first element (alias for `\\`). */
  def downArray: ACursor =
    history.acursorElement(Store(_.downArray, cursor), CursorOpDownArray)

  /** Move the cursor down to a JSON array at the first element (alias for `downArray`). */
  def \\ : ACursor =
    downArray

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `downAt`). */
  def -\(p: Json => Boolean): ACursor =
    downAt(p)

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `-\`). */
  def downAt(p: Json => Boolean): ACursor =
    history.acursorElement(Store(_.downAt(p), cursor), CursorOpDownAt(p))

  /** Move the cursor down to a JSON array at the given index (alias for `downN`). */
  def =\(n: Int): ACursor =
    downN(n)

  /** Move the cursor down to a JSON array at the given index (alias for `=\`). */
  def downN(n: Int): ACursor =
    history.acursorElement(Store(_.downN(n), cursor), CursorOpDownN(n))

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def delete : ACursor =
    deleteGoParent

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def unary_! : ACursor =
    deleteGoParent

  /** Deletes the JSON value at focus and moves up to parent (alias for `unary_!`). */
  def deleteGoParent: ACursor =
    history.acursorElement(Store(_.deleteGoParent, cursor), CursorOpDeleteGoParent)

  /** Deletes the JSON value at focus and moves to the left in a JSON array. */
  def deleteGoLeft: ACursor =
    history.acursorElement(Store(_.deleteGoLeft, cursor), CursorOpDeleteGoLeft)

  /** Deletes the JSON value at focus and moves to the right in a JSON array. */
  def deleteGoRight: ACursor =
    history.acursorElement(Store(_.deleteGoRight, cursor), CursorOpDeleteGoRight)

  /** Deletes the JSON value at focus and moves to the first in a JSON array. */
  def deleteGoFirst: ACursor =
    history.acursorElement(Store(_.deleteGoFirst, cursor), CursorOpDeleteGoFirst)

  /** Deletes the JSON value at focus and moves to the last in a JSON array. */
  def deleteGoLast: ACursor =
    history.acursorElement(Store(_.deleteGoLast, cursor), CursorOpDeleteGoLast)

  /** Deletes the JSON value at focus and moves to the given sibling field in a JSON object. */
  def deleteGoField(q: JsonField): ACursor =
    history.acursorElement(Store(_.deleteGoField(q), cursor), CursorOpDeleteGoField(q))

  /** Deletes all JSON values to left of focus in a JSON array. */
  def deleteLefts: ACursor =
    history.acursorElement(Store(_.deleteLefts, cursor), CursorOpDeleteLefts)

  /** Deletes all JSON values to right of focus in a JSON array. */
  def deleteRights: ACursor =
    history.acursorElement(Store(_.deleteRights, cursor), CursorOpDeleteRights)

  /** Set the values to the left of focus in a JSON array. */
  def setLefts(x: List[Json]): ACursor =
    history.acursorElement(Store(_.setLefts(x), cursor), CursorOpDeleteRights)

  /** Set the values to the right of focus in a JSON array. */
  def setRights(x: List[Json]): ACursor =
    history.acursorElement(Store(_.setRights(x), cursor), CursorOpDeleteRights)

  /** Move the cursor up one step to the parent context. */
  def up: ACursor =
    history.acursorElement(Store(_.up, cursor), CursorOpUp)

  // (HCursor, X) => (Option[HCursor], X) => X => X
  def traverseBreak[X](r: Kleisli[({type λ[+α] = State[X, α]})#λ, HCursor, Option[HCursor]]): Endo[X] =
    Endo(x => {
      @annotation.tailrec
      def spin(z: X, d: HCursor): X = {
        val (q, k) = r run d run z
        k match {
          case None => q
          case Some(a) => spin(q, a)
        }
      }

      spin(x, this)
    })

  def traverse[X](r: Kleisli[({type λ[+α] = State[X, α]})#λ, HCursor, HCursor]): Endo[X] =
    traverseBreak(r map (Some(_)))

  def traverseABreak[X](r: Kleisli[({type λ[+α] = State[X, α]})#λ, HCursor, Option[ACursor]]): State[X, Boolean] =
    State(x => {
      @annotation.tailrec
      def spin(z: X, d: HCursor): (X, Boolean) = {
        val (q, k) = r run d run z
        k match {
          case None => (q, true)
          case Some(a) =>
            if(a.succeeded)
              spin(q, a.hcursor)
            else
              (q, false)
        }
      }

      spin(x, this)
    })

  def traverseA[X](r: Kleisli[({type λ[+α] = State[X, α]})#λ, HCursor, ACursor]): State[X, Boolean] =
    traverseABreak(r map (Some(_)))

}

object HCursor extends HCursors {
  def apply(c: Cursor, h: CursorHistory): HCursor =
    new HCursor {
      val cursor = c
      val history = h
    }
}

trait HCursors {
  val hcursorL: HCursor @> Cursor =
    Lens(c => Store(HCursor(_, c.history), c.cursor))

  val hcursorHistoryL: HCursor @> CursorHistory =
    Lens(c => Store(HCursor(c.cursor, _), c.history))
}
