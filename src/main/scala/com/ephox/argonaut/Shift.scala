package com.ephox
package argonaut

import scalaz._, Scalaz._, Free._
import Json._

/**
 * A data structure that shifts a JSON cursor around, while keeping history.
 *
 * @see Cursor
 * @author Tony Morris
 */
sealed trait Shift {
  def shift(c: Cursor): Trampoline[ShiftResult]

  /**
   * Run with the given cursor.
   */
  def run(c: Cursor): ShiftResult =
    shift(c).run

  /**
   * Run with a cursor on the given JSON value.
   */
  def <|(j: Json): ShiftResult =
    run(+j)

  /**
   * Run with the given cursor and see only history.
   */
  def history(c: Cursor): ShiftHistory =
    run(c).history

  /**
   * Run with the given cursor and see the new cursor (drop history).
   */
  def cursor(c: Cursor): Option[Cursor] =
    run(c).cursor

  /**
   * Run with the given cursor and if the cursor fails, return the given cursor.
   */
  def or(c: Cursor): Cursor =
    cursor(c) getOrElse c

  /** Update the focus with the given function (alias for `withFocus`). */
  def >->(k: Json => Json): Shift =
    withFocus(k)

  /** Update the focus with the given function (alias for `>->`). */
  def withFocus(k: Json => Json): Shift =
    Shift(c =>
      shift(c) map ((ShiftResult.resultCursorL andThen ~Cursor.focusL) =>= k))

  /** Set the focus to the given value (alias for `set`). */
  def :=(q: => Json): Shift =
    set(q)

  /** Set the focus to the given value (alias for `:=`). */
  def set(q: => Json): Shift =
    withFocus(_ => q)
  /**
   * Run this cursor-shift then the given cursor-shift.
   */
  def compose(s: => Shift): Shift =
    Shift(c =>
      shift(c) flatMap (q => q.cursor match {
        case None => implicitly[Monad[Trampoline]].point(q)
        case Some(cc) => s shift cc map (ShiftResult.resultHistoryL =>= (q.history ++ _))
      }))

  /**
   * Run the given cursor-shift and then this cursor-shift.
   */
  def andThen(s: Shift): Shift =
    s compose this

  /**
   * Repeat this cursor-shift until last point before failure.
   */
  def repeat: Shift =
    this compose repeat ||| this

  /**
   * Run the cursor-shift and always succeed, with the original cursor if it has to (alias for `unary_~`).
   */
  def attempt: Shift =
    Shift(c => shift(c) map (i => ShiftResult(i.history, i.collapse)))

  /**
   * Run the cursor-shift and always succeed, with the original cursor if it has to (alias for `attempt`).
   */
  def unary_~ : Shift =
    attempt

  /**
   * Run the cursor-shift the given number of times (alias for `times`).
   */
  def %(n: Int): Shift =
    times(n)

  /**
   * Run the cursor-shift the given number of times (alias for `%`).
   */
  def times(n: Int): Shift = {
    @annotation.tailrec
    def go(n: Int, acc: Shift): Shift =
      if (n <= 0)
        acc
      else
        go(n - 1, this compose acc)

    go(n, this)
  }

  /**
   * Shift left in a JSON array.
   */
  def left: Shift =
    this compose Shift.tramps(c => (ShiftLeft, c.left))

  /**
   * Shift right in a JSON array.
   */
  def right: Shift =
    this compose Shift.tramps(c => (ShiftRight, c.right))

  /**
   * Shift to the first in a JSON array.
   */
  def first: Shift =
    this compose Shift.tramps(c => (ShiftFirst, c.first))

  /**
   * Shift to the last in a JSON array.
   */
  def last: Shift =
    this compose Shift.tramps(c => (ShiftLast, c.last))

  /**
   * Shift left in a JSON array the given number of times (alias for `leftN`).
   */
  def -<-:(n: Int): Shift =
    leftN(n)

  /**
   * Shift left in a JSON array the given number of times (alias for `-<-`).
   */
  def leftN(n: Int): Shift =
    this compose Shift.tramps(c => (ShiftLeftN(n), n -<-: c))

  /**
   * Shift right in a JSON array the given number of times (alias for `rightN`).
   */
  def ->-:(n: Int): Shift =
    rightN(n)

  /**
   * Shift right in a JSON array the given number of times (alias for `->-`).
   */
  def rightN(n: Int): Shift =
    this compose Shift.tramps(c => (ShiftRightN(n), c :->- n))

  /**
   * Shift left in a JSON array until the given predicate matches (alias for `leftAt`).
   */
  def ?<-:(p: Json => Boolean): Shift =
    leftAt(p)

  /**
   * Shift left in a JSON array until the given predicate matches (alias for `?<-:`).
   */
  def leftAt(p: Json => Boolean): Shift =
    this compose Shift.tramps(c => (ShiftLeftAt(p), p ?<-: c))

  /**
   * Shift right in a JSON array until the given predicate matches (alias for `rightAt`).
   */
  def :->?(p: Json => Boolean): Shift =
    rightAt(p)

  /**
   * Shift right in a JSON array until the given predicate matches (alias for `:->?`).
   */
  def rightAt(p: Json => Boolean): Shift =
    this compose Shift.tramps(c => (ShiftRightAt(p), c :->? p))

  /**
   * Shift to the given sibling field in a JSON object (alias for `field`).
   */
  def --(f: JsonField): Shift =
    field(f)

  /**
   * Shift to the given sibling field in a JSON object (alias for `--`).
   */
  def field(f: JsonField): Shift =
    this compose Shift.tramps(c => (ShiftField(f), c -- f))

  /**
   * Shift down to a JSON object at the given field (alias for `downField`).
   */
  def --\(f: JsonField): Shift =
    downField(f)

  /**
   * Shift down to a JSON object at the given field (alias for `--\`).
   */
  def downField(f: JsonField): Shift =
    this compose Shift.tramps(c => (ShiftDownField(f), c --\ f))

  /**
   * Shift down to a JSON array at the first element (alias for `downArray`).
   */
  def \\ : Shift =
    downArray

  /**
   * Shift down to a JSON array at the first element (alias for `\\`).
   */
  def downArray: Shift =
    this compose Shift.tramps(c => (ShiftDownArray, c.downArray))

  /**
   * Shift down to a JSON array at the first element satisfying the given predicate (alias for `downAt`).
   */
  def -\(p: Json => Boolean): Shift =
    downAt(p)

  /**
   * Shift down to a JSON array at the first element satisfying the given predicate (alias for `-\`).
   */
  def downAt(p: Json => Boolean): Shift =
    this compose Shift.tramps(c => (ShiftDownAt(p), c -\ p))

  /**
   * Shift down to a JSON array at the given index (alias for `downN`).
   */
  def =\(n: Int): Shift =
    downN(n)

  /**
   * Shift down to a JSON array at the given index (alias for `=\`).
   */
  def downN(n: Int): Shift =
    this compose Shift.tramps(c => (ShiftDownN(n), c =\ n))

  /**
   * Deletes at focus and shifts up to parent (alias for `deleteGoParent`).
   */
  def delete: Shift =
    deleteGoParent

  /**
   * Deletes at focus and shifts up to parent (alias for `deleteGoParent`).
   */
  def unary_! : Shift =
    deleteGoParent

  /**
   * Deletes at focus and shifts up to parent (alias for `unary_!`).
   */
  def deleteGoParent: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoParent, !c))

  /**
   * Deletes at focus and shifts left in a JSON array.
   */
  def deleteGoLeft: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoLeft, c.deleteGoLeft))

  /**
   * Deletes at focus and shifts right in a JSON array.
   */
  def deleteGoRight: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoRight, c.deleteGoRight))

  /**
   * Deletes at focus and shifts to the first in a JSON array.
   */
  def deleteGoFirst: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoFirst, c.deleteGoFirst))

  /**
   * Deletes at focus and shifts to the last in a JSON array.
   */
  def deleteGoLast: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoLast, c.deleteGoLast))

  /**
   * Deletes at focus and shifts to the given sibling field in a JSON object.
   */
  def deleteGoField(f: JsonField): Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoField(f), c.deleteGoField(f)))

  /**
   * Shift to immediate parent.
   */
  def up: Shift =
    this compose Shift.tramps(c => (ShiftUp, c.up))

  /**
   * Try this cursor-shift. If it fails, try the given one.
   */
  def |||(s: => Shift): Shift =
    Shift(c =>
      shift(c) flatMap (z => z.previousOrCursor match {
        case Left(d) => s shift d
        case Right(_) => implicitly[Monad[Trampoline]].point(z)
      }))

  /**
   * Fail the cursor-shift if the given predicate fails.
   */
  def whenC(p: Cursor => Boolean): Shift =
    Shift(c =>
      if(p(c))
        shift(c)
      else
        implicitly[Monad[Trampoline]].point(ShiftResult.previousResult(Monoid[ShiftHistory].zero, c)))

  /**
   * Fail the cursor-shift if the given predicate fails (alias for `?`).
   */
  def when(p: Json => Boolean): Shift =
    whenC(c => p(c.focus))

  /**
   * Fail the cursor-shift if the given predicate fails (alias for `when`).
   */
  def ?(p: Json => Boolean): Shift =
    when(p)

  /**
   * Fail the cursor-shift if the given predicate succeeds.
   */
  def unlessC(p: Cursor => Boolean): Shift =
    whenC(c => !p(c))

  /**
   * Fail the cursor-shift if the given predicate succeeds alias for `!?`).
   */
  def unless(p: Json => Boolean): Shift =
    unlessC(c => p(c.focus))

  /**
   * Fail the cursor-shift if the given predicate succeeds alias for `unless`).
   */
  def !?(p: Json => Boolean): Shift =
    unless(p)
}

object Shift extends Shifts {
  def apply(k: Cursor => Trampoline[ShiftResult]): Shift =
    new Shift {
      def shift(c: Cursor) = k(c)
    }

  private[argonaut] def tramps(f: Cursor => (ShiftHistoryElement, Option[Cursor])): Shift =
    Shift(c => {
      val (e, q) = f(c)
      implicitly[Monad[Trampoline]].point(ShiftResult.shiftResult(ShiftHistory(e), q.toRight(c)))
    })
}

trait Shifts {

  /**
   * A succeeding cursor-shift with no history.
   */
  def shift: Shift =
    Shift(c => implicitly[Monad[Trampoline]].point(ShiftResult(Monoid[ShiftHistory].zero, c)))

  implicit val ShiftInstances: Monoid[Shift] =
    new Monoid[Shift] {
      def append(s1: Shift, s2: => Shift): Shift =
        s1 compose s2
      def zero =
        shift
    }
}
