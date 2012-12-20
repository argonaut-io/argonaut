package argonaut

import scalaz._, Scalaz._

sealed trait ACursor {
  val succeeded: Boolean
  val hcursor: HCursor

  import ACursor._
  import HCursor._
  import CursorOp._
  import Json._

  /**
   * Attempts to decode this cursor focus value to another data type.
   */
  def jdecode[A](implicit e: DecodeJson[A]): DecodeResult[A] =
    hcursor.jdecode[A]

  /**
   * Attempts to move down onto a field `name` and decode the focus.
   */
  def get[A](name: String)(implicit e: DecodeJson[A]): DecodeResult[A] =
    hcursor.get[A](name)

  def failed: Boolean =
    !succeeded

  def cursor: Cursor =
    hcursor.cursor

  def history: CursorHistory =
    hcursor.history

  def reattempt: ACursor =
    ACursor.build(hcursorHistoryL.mod(reattemptOp +: _, hcursor), true)

  def unary_~ : ACursor =
    reattempt

  /** Return the current focus. */
  def focus: Json =
    cursor.focus

  /** Update the focus with the given function (alias for `withFocus`). */
  def >->(k: Json => Json): ACursor =
    withFocus(k)

  /** Update the focus with the given function (alias for `>->`). */
  def withFocus(k: Json => Json): ACursor =
    ACursor.build(hcursor withFocus k, succeeded)

  /** Update the focus with the given function in a functor (alias for `withFocusM`). */
  def >-->[F[+_]: Functor](k: Json => F[Json]): F[ACursor] =
    withFocusM(k)

  /** Update the focus with the given function in a functor (alias for `>-->`). */
  def withFocusM[F[+_]: Functor](k: Json => F[Json]): F[ACursor] =
    Functor[F].map(hcursor withFocusM k)(ACursor.build(_, succeeded))

  /** Set the focus to the given value (alias for `:=`). */
  def set(j: Json): ACursor =
    withFocus(_ => j)

  /** Set the focus to the given value (alias for `set`). */
  def :=(j: Json): ACursor =
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

  /** Move the cursor left in a JSON array. */
  def left: ACursor =
    hcursor.left

  /** Move the cursor right in a JSON array. */
  def right: ACursor =
    hcursor.right

  /** Move the cursor to the first in a JSON array. */
  def first: ACursor =
    hcursor.first

  /** Move the cursor to the last in a JSON array. */
  def last: ACursor =
    hcursor.last

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `leftN`). */
  def -<-:(n: Int): ACursor =
    hcursor.-<-:(n)

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `-<-:`). */
  def leftN(n: Int): ACursor =
    hcursor.leftN(n)

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `rightN`). */
  def :->-(n: Int): ACursor =
    hcursor.:->-(n)

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `:->-`). */
  def rightN(n: Int): ACursor =
    hcursor.rightN(n)

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `leftAt`). */
  def ?<-:(p: Json => Boolean): ACursor =
    hcursor.?<-:(p)

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `?<-:`). */
  def leftAt(p: Json => Boolean): ACursor =
    hcursor.leftAt(p)

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `rightAt`). */
  def :->?(p: Json => Boolean): ACursor =
    hcursor.:->?(p)

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `:->?`). */
  def rightAt(p: Json => Boolean): ACursor =
    hcursor.rightAt(p)

  /** Move the cursor to the given sibling field in a JSON object (alias for `field`). */
  def --(q: JsonField): ACursor =
    hcursor.--(q)

  /** Move the cursor to the given sibling field in a JSON object (alias for `--`). */
  def field(q: JsonField): ACursor =
    hcursor.field(q)

  /** Move the cursor down to a JSON object at the given field (alias for `downField`). */
  def --\(q: JsonField): ACursor =
    hcursor.--\(q)

  /** Move the cursor down to a JSON object at the given field (alias for `--\`). */
  def downField(q: JsonField): ACursor =
    hcursor.downField(q)

  /** Move the cursor down to a JSON array at the first element (alias for `\\`). */
  def downArray: ACursor =
    hcursor.downArray

  /** Move the cursor down to a JSON array at the first element (alias for `downArray`). */
  def \\ : ACursor =
    hcursor.\\

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `downAt`). */
  def -\(p: Json => Boolean): ACursor =
    hcursor.-\(p)

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `-\`). */
  def downAt(p: Json => Boolean): ACursor =
    hcursor.downAt(p)

  /** Move the cursor down to a JSON array at the given index (alias for `downN`). */
  def =\(n: Int): ACursor =
    hcursor.=\(n)

  /** Move the cursor down to a JSON array at the given index (alias for `=\`). */
  def downN(n: Int): ACursor =
    hcursor.downN(n)

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def delete : ACursor =
    hcursor.delete

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def unary_! : ACursor =
    hcursor.unary_!

  /** Deletes the JSON value at focus and moves up to parent (alias for `unary_!`). */
  def deleteGoParent: ACursor =
    hcursor.deleteGoParent

  /** Deletes the JSON value at focus and moves to the left in a JSON array. */
  def deleteGoLeft: ACursor =
    hcursor.deleteGoLeft

  /** Deletes the JSON value at focus and moves to the right in a JSON array. */
  def deleteGoRight: ACursor =
    hcursor.deleteGoRight

  /** Deletes the JSON value at focus and moves to the first in a JSON array. */
  def deleteGoFirst: ACursor =
    hcursor.deleteGoFirst

  /** Deletes the JSON value at focus and moves to the last in a JSON array. */
  def deleteGoLast: ACursor =
    hcursor.deleteGoLast

  /** Deletes the JSON value at focus and moves to the given sibling field in a JSON object. */
  def deleteGoField(q: JsonField): ACursor =
    hcursor.deleteGoField(q)

  /** Deletes all JSON values to left of focus in a JSON array. */
  def deleteLefts: ACursor =
    hcursor.deleteLefts

  /** Deletes all JSON values to right of focus in a JSON array. */
  def deleteRights: ACursor =
    hcursor.deleteRights

  /** Set the values to the left of focus in a JSON array. */
  def setLefts(x: List[Json]): ACursor =
    hcursor.setLefts(x)

  /** Set the values to the right of focus in a JSON array. */
  def setRights(x: List[Json]): ACursor =
    setRights(x)

  /** Move the cursor up one step to the parent context. */
  def up: ACursor =
    hcursor.up

  def |||(c: => ACursor): ACursor =
    if(succeeded)
      this
    else
      c

  def validation: Validation[HCursor, HCursor] =
    if(succeeded)
      Success(hcursor)
    else
      Failure(hcursor)

  def unary_- : Validation[HCursor, HCursor]  =
    validation

  def either: Either[HCursor, HCursor] =
    if(succeeded)
      Right(hcursor)
    else
      Left(hcursor)

  def success: Option[HCursor] =
    validation.toOption

  def failure: Option[HCursor] =
    validation.fail.toOption

}

object ACursor extends ACursors {
  def apply(c: HCursor): ACursor =
    build(c, true)
}

trait ACursors {
  private[argonaut] def build(h: HCursor, s: Boolean): ACursor =
    new ACursor {
      val succeeded = s
      val hcursor = h
    }

  def failedACursor(h: HCursor): ACursor =
    build(h, false)

  val acursorHCursorL: ACursor @> HCursor =
    Lens(c => Store(h => build(h, c.succeeded), c.hcursor))

  val acursorCursorL: ACursor @> Cursor =
    HCursor.hcursorL compose acursorHCursorL

  val acursorHistoryL: ACursor @> CursorHistory =
    HCursor.hcursorHistoryL compose acursorHCursorL

  val acursorSucceededL: ACursor @> Boolean =
    Lens(c => Store(build(c.hcursor, _), c.succeeded))
}
