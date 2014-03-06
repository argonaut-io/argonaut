package argonaut

import scalaz._, Scalaz._

case class HCursor(cursor: Cursor, history: CursorHistory) {
  import Json._

  def acursor: ACursor =
    ACursor.ok(this)

  /**
   * Attempts to decode this cursor focus value to another data type.
   */
  def jdecode[A](implicit e: DecodeJson[A]): DecodeResult[A] =
    e.decode(this)

  /**
   * Attempts to decode this cursor focus value to another data type.
   * Alias for `jdecode`.
   */
  def as[A](implicit e: DecodeJson[A]): DecodeResult[A] =
    jdecode[A]

  /**
   * Attempts to move down onto a field `name` and decode the focus.
   */
  def get[A](name: String)(implicit e: DecodeJson[A]): DecodeResult[A] =
    (this --\ name).as[A]

  def failedACursor: ACursor =
    ACursor.fail(this)

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

  /** Find the first element at or to the right of focus in a JSON array where the given predicate matches the focus. */
  def find(p: Json => Boolean): ACursor =
    history.acursorElement(Store(_.find(p), cursor), CursorOpRightAt(p))

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

  /** Unapplies the cursor to the top-level parent (alias for `undo`). */
  def unary_- : Json =
    cursor.undo

  /** Unapplies the cursor to the top-level parent (alias for `unary_-`). */
  def undo: Json =
    cursor.undo

  // FIX These are rubbish (that is all of these traverse operations).
  //     Revisit and tidy. Option[ACursor] in particular doesn't
  //     represent anything meaningful, the client has to set the
  //     termination state either None or Some(!_.succeeded), but
  //     could just have one since it can represent success, failure
  //     or indeference in the accumulator (X).
  //
  //     For now I have added some specialisations to get rid of a
  //     bunch of dupe. But further work is required.

  def traverseBreak[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, HCursor, Option[HCursor]]): Endo[X] =
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

  def traverse[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, HCursor, HCursor]): Endo[X] =
    traverseBreak(r map (Some(_)))

  def traverseABreak[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, HCursor, Option[ACursor]]): State[X, Boolean] =
    State(x => {
      @annotation.tailrec
      def spin(z: X, d: HCursor): (X, Boolean) = {
        val (q, k) = r run d run z
        k match {
          case None => (q, true)
          case Some(a) =>
            a.hcursor match {
              case None => (q, false)
              case Some(hcursor) => spin(q, hcursor)
            }
        }
      }

      spin(x, this)
    })

  def traverseA[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, HCursor, ACursor]): State[X, Boolean] =
    traverseABreak(r map (Some(_)))

  /**
   * Traverse until either `f` does not return a cursor or the cursor did not succeed,
   * accumulating X at each sterp
   */
  def traverseUntil[X](init: X)(f: (X, HCursor) => (X, Option[ACursor])): X =
    traverseABreak[X](Kleisli[({type λ[+α] = State[X, α]})#λ, HCursor, Option[ACursor]](c => State((x: X) => f(x, c)))) exec init

  /**
   * Traverse until `f` returns a cursor that did not succeed,
   * accumulating X at each step
   */
  def traverseUntilDone[X](init: X)(f: (X, HCursor) => (X, ACursor)): X =
    traverseUntil(init)((x, c) => { val (xx, cc) = f(x, c); (xx, Some(cc)) })

  /**
   * Traverse taking `op` at each step, performing `f` on the current cursor
   * and accumulate X through DecodeResult.
   *
   * This operation does not consume stack at each step, so is safe to
   * work with large structures (that is compared with recursive flatMap).
   */
  def traverseDecode[X](init: X)(op: HCursor => ACursor, f: (X, HCursor) => DecodeResult[X]) =
    f(init, this).map(x => (this, x)).loop[DecodeResult[X]](
      DecodeResult.fail, { case (c, acc) =>
        val a = op(c)
        a.hcursor match {
          case None => acc.pure[DecodeResult].left
          case Some(hcursor) => (f(acc, hcursor) map (x => (hcursor, x))).right
        }
      })
}

object HCursor extends HCursors

trait HCursors {
  val hcursorL: HCursor @> Cursor =
    Lens(c => Store(HCursor(_, c.history), c.cursor))

  val hcursorHistoryL: HCursor @> CursorHistory =
    Lens(c => Store(HCursor(c.cursor, _), c.history))
}
