package com.ephox
package argonaut

import scalaz._, Scalaz._

/**
 * The pair of cursor-shift history and the current cursor position.
 *
 * @see Shift
 * @author Tony Morris
 */
sealed trait ShiftResult {
  /**
   * The cursor-shift history.
   */
  val history: ShiftHistory

  val previousOrCursor: Either[Cursor, Cursor]

  /**
   * Return either previous cursor or cursor.
   */
  def collapse: Cursor =
    previousOrCursor match {
      case Left(c) => c
      case Right(c) => c
    }

  /**
   * The previous cursor if there is no current cursor.
   */
  def previous: Option[Cursor] =
    previousOrCursor.left.toOption

  /**
   * The current cursor position if it has succeeded.
   */
  def cursor: Option[Cursor] =
    previousOrCursor.right.toOption

  /**
   * Return whether or not the current cursor position has succeeded.
   */
  def hasCursor =
    cursor.isDefined

  /**
   * Return the succeeded cursor position or the given default.
   */
  def cursorOr(c: => Cursor): Cursor =
    cursor getOrElse c

  /**
   * Swap the cursor with the previous.
   */
  def unary_~ : ShiftResult =
    new ShiftResult {
      val history = ShiftResult.this.history
      val previousOrCursor = ShiftResult.this.previousOrCursor.swap
    }
}

object ShiftResult extends ShiftResults {
  def apply(h: ShiftHistory, c: Cursor): ShiftResult =
    new ShiftResult {
      val history = h
      val previousOrCursor = Right(c)
    }
}

trait ShiftResults {
  implicit val ShiftResultInstances: Equal[ShiftResult] with Show[ShiftResult] =
    new Equal[ShiftResult] with Show[ShiftResult] {
      def equal(r1: ShiftResult, r2: ShiftResult) =
        Equal.equalBy((r: ShiftResult) => (r.history, r.cursor)).equal(r1, r2)
      def show(r: ShiftResult) =
        ("{ history=" + r.history.shows + (r.cursor match {
          case None => ""
          case Some(c) => ", cursor=" + c.shows
        }) + " }").toList
    }

  private[argonaut] def shiftResult(h: ShiftHistory, c: Either[Cursor, Cursor]): ShiftResult =
    new ShiftResult {
      val history = h
      val previousOrCursor = c
    }

  /**
   * Construct a cursor-shift result without a cursor, using the given previous.
   */
  def previousResult(h: ShiftHistory, c: Cursor): ShiftResult =
    new ShiftResult {
      val history = h
      val previousOrCursor = Left(c)
    }

  /**
   * The lens on the cursor-shift history.
   */
  val resultHistoryL: ShiftResult @> ShiftHistory =
    Lens(r => Store(shiftResult(_, r.previousOrCursor), r.history))

  /**
   * The lens on the cursor-shift cursor position.
   */
  val resultCursorL: ShiftResult @?> Cursor =
    PLens(r => r.cursor map (q => Store(ShiftResult(r.history, _), q)))

  /**
   * The lens on the cursor-shift previous cursor position.
   */
  val resultPreviousL: ShiftResult @?> Cursor =
    PLens(r => r.cursor map (q => Store(previousResult(r.history, _), q)))

}
