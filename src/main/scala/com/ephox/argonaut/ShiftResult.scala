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

  /**
   * The current cursor position if it has succeeded.
   */
  val cursor: Option[Cursor]

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
}

object ShiftResult extends ShiftResults {
  def apply(h: ShiftHistory, c: Option[Cursor]): ShiftResult =
    new ShiftResult {
      val history = h
      val cursor = c
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

  /**
   * The lens on the cursor-shift history.
   */
  def resultHistoryL: ShiftResult @> ShiftHistory =
    Lens(r => Store(ShiftResult(_, r.cursor), r.history))

  /**
   * The lens on the cursor-shift cursor position.
   */
  def resultCursorL: ShiftResult @> Option[Cursor] =
    Lens(r => Store(ShiftResult(r.history, _), r.cursor))

  /**
   * The partial lens on the cursor-shift cursor position.
   */
  def resultCursorPL: ShiftResult @?> Cursor =
    PLensT.somePLens compose ~resultCursorL

}
