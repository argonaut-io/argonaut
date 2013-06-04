package argonaut

import scalaz._, Scalaz._

case class ACursor(either: HCursor \/ HCursor) {
  import ACursor._
  import HCursor._
  import CursorOp._
  import Json._

  /** Get the current hcursor if we are in an succeeded state. Alias for `success`. */
  def hcursor: Option[HCursor] =
    either.toOption

  /** Get the current hcursor if we are in an succeeded state. Alias for `hcursor`. */
  def success: Option[HCursor] =
    hcursor

  /** Get the failed hcursor if we are in an failed state. Alias for `hcursor`. */
  def failure: Option[HCursor] =
    either.swap.toOption

  /**
   * Attempts to decode this cursor focus value to another data type.
   */
  def jdecode[A](implicit e: DecodeJson[A]): DecodeResult[A] =
    e.tryDecode(this)

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
    either match {
      case -\/(invalid) => DecodeResult.fail("Attempt to decode value on failed cursor.", invalid.history)
      case \/-(valid) => valid.get[A](name)
    }

  def succeeded: Boolean =
    hcursor.isDefined

  def failed: Boolean =
    !succeeded

  def cursor: Option[Cursor] =
    hcursor.map(_.cursor)

  def any: HCursor =
    either.fold(l => l, r => r)

  def history: CursorHistory =
    any.history

  def reattempt: ACursor =
    either match {
      case -\/(invalid) => ACursor.ok(hcursorHistoryL.mod(reattemptOp +: _, invalid))
      case \/-(_) => this
    }

  def unary_~ : ACursor =
    reattempt

  /** Return the current focus, iff we are succeeded */
  def focus: Option[Json] =
    success.map(_.focus)

  /** Return the previous focus, iff we are !succeeded. */
  def failureFocus: Option[Json] =
    failure.map(_.focus)

  /** Update the focus with the given function (alias for `withFocus`). */
  def >->(k: Json => Json): ACursor =
    withFocus(k)

  /** Update the focus with the given function (alias for `>->`). */
  def withFocus(k: Json => Json): ACursor =
    ACursor(either.map(_.withFocus(k)))

  /** Update the focus with the given function in a applicative (alias for `withFocusM`). */
  def >-->[F[+_]: Applicative](k: Json => F[Json]): F[ACursor] =
    withFocusM(k)



  /** Update the focus with the given function in a applicative (alias for `>-->`). */
  def withFocusM[F[+_]: Applicative](k: Json => F[Json]): F[ACursor] =
    either match {
      case -\/(invalid) => this.point[F]
      case \/-(valid) => Functor[F].map(valid withFocusM k)(ACursor.ok(_))
    }

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
    hcursor.flatMap(_.lefts)

  /**
   * Return the values right of focus in a JSON array.
   */
  def rights: Option[JsonArray] =
    hcursor.flatMap(_.rights)

  def withHCursor(f: HCursor => ACursor): ACursor =
    ACursor(either.flatMap(c => f(c).either))

  /** Move the cursor left in a JSON array. */
  def left: ACursor =
    withHCursor(_.left)

  /** Move the cursor right in a JSON array. */
  def right: ACursor =
    withHCursor(_.right)

  /** Move the cursor to the first in a JSON array. */
  def first: ACursor =
    withHCursor(_.first)

  /** Move the cursor to the last in a JSON array. */
  def last: ACursor =
    withHCursor(_.last)

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `leftN`). */
  def -<-:(n: Int): ACursor =
    withHCursor(c => n -<-: c)

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right (alias for `-<-:`). */
  def leftN(n: Int): ACursor =
    withHCursor(_.leftN(n))

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `rightN`). */
  def :->-(n: Int): ACursor =
    withHCursor(c => c :->- n)

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left (alias for `:->-`). */
  def rightN(n: Int): ACursor =
    withHCursor(_.rightN(n))

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `leftAt`). */
  def ?<-:(p: Json => Boolean): ACursor =
    withHCursor(c => p ?<-: c)

  /** Move the cursor left in a JSON array until the given predicate matches the focus (alias for `?<-:`). */
  def leftAt(p: Json => Boolean): ACursor =
    withHCursor(_.leftAt(p))

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `rightAt`). */
  def :->?(p: Json => Boolean): ACursor =
    withHCursor(c => c :->? p)

  /** Move the cursor right in a JSON array until the given predicate matches the focus (alias for `:->?`). */
  def rightAt(p: Json => Boolean): ACursor =
    withHCursor(_.rightAt(p))

  /** Move the cursor to the given sibling field in a JSON object (alias for `field`). */
  def --(q: JsonField): ACursor =
    withHCursor(c => c -- q)

  /** Move the cursor to the given sibling field in a JSON object (alias for `--`). */
  def field(q: JsonField): ACursor =
    withHCursor(_.field(q))

  /** Move the cursor down to a JSON object at the given field (alias for `downField`). */
  def --\(q: JsonField): ACursor =
    withHCursor(c => c --\ q)

  /** Move the cursor down to a JSON object at the given field (alias for `--\`). */
  def downField(q: JsonField): ACursor =
    withHCursor(_.downField(q))

  /** Move the cursor down to a JSON array at the first element (alias for `\\`). */
  def downArray: ACursor =
    withHCursor(_.downArray)

  /** Move the cursor down to a JSON array at the first element (alias for `downArray`). */
  def \\ : ACursor =
    withHCursor(_.\\)

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `downAt`). */
  def -\(p: Json => Boolean): ACursor =
    withHCursor(c => c -\ p)

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate (alias for `-\`). */
  def downAt(p: Json => Boolean): ACursor =
    withHCursor(_.downAt(p))

  /** Move the cursor down to a JSON array at the given index (alias for `downN`). */
  def =\(n: Int): ACursor =
    withHCursor(c => c =\ n)

  /** Move the cursor down to a JSON array at the given index (alias for `=\`). */
  def downN(n: Int): ACursor =
    withHCursor(_.downN(n))

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def delete : ACursor =
    withHCursor(_.delete)

  /** Deletes the JSON value at focus and moves up to parent (alias for `deleteGoParent`). */
  def unary_! : ACursor =
    withHCursor(_.unary_!)

  /** Deletes the JSON value at focus and moves up to parent (alias for `unary_!`). */
  def deleteGoParent: ACursor =
    withHCursor(_.deleteGoParent)

  /** Deletes the JSON value at focus and moves to the left in a JSON array. */
  def deleteGoLeft: ACursor =
    withHCursor(_.deleteGoLeft)

  /** Deletes the JSON value at focus and moves to the right in a JSON array. */
  def deleteGoRight: ACursor =
    withHCursor(_.deleteGoRight)

  /** Deletes the JSON value at focus and moves to the first in a JSON array. */
  def deleteGoFirst: ACursor =
    withHCursor(_.deleteGoFirst)

  /** Deletes the JSON value at focus and moves to the last in a JSON array. */
  def deleteGoLast: ACursor =
    withHCursor(_.deleteGoLast)

  /** Deletes the JSON value at focus and moves to the given sibling field in a JSON object. */
  def deleteGoField(q: JsonField): ACursor =
    withHCursor(_.deleteGoField(q))

  /** Deletes all JSON values to left of focus in a JSON array. */
  def deleteLefts: ACursor =
    withHCursor(_.deleteLefts)

  /** Deletes all JSON values to right of focus in a JSON array. */
  def deleteRights: ACursor =
    withHCursor(_.deleteRights)

  /** Set the values to the left of focus in a JSON array. */
  def setLefts(x: List[Json]): ACursor =
    withHCursor(_.setLefts(x))

  /** Set the values to the right of focus in a JSON array. */
  def setRights(x: List[Json]): ACursor =
    withHCursor(_.setRights(x))

  /** Move the cursor up one step to the parent context. */
  def up: ACursor =
    withHCursor(_.up)

  def |||(c: => ACursor): ACursor =
    if(succeeded)
      this
    else
      c

  def validation: Validation[HCursor, HCursor] =
    either.validation

  /** Unapplies the cursor to the top-level parent (alias for `undo`). */
  def unary_- : Option[Json] =
    hcursor.map(_.undo)

  /** Unapplies the cursor to the top-level parent (alias for `unary_-`). */
  def undo: Option[Json] =
    hcursor.map(_.undo)
}

object ACursor extends ACursors {
  def ok(cursor: HCursor) =
    ACursor(\/-(cursor))

  def fail(cursor: HCursor) =
    ACursor(-\/(cursor))
}

trait ACursors {
  def okACursor(cursor: HCursor) =
    ACursor.ok(cursor)

  def failACursor(cursor: HCursor) =
    ACursor.fail(cursor)
}
