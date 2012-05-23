package com.ephox
package argonaut

import scalaz._, Scalaz._

sealed trait ShiftResult {
  val history: ShiftHistory
  val cursor: Option[Cursor]

  def hasCursor =
    cursor.isDefined

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

  def resultHistoryL: ShiftResult @> ShiftHistory =
    Lens(r => Costate(ShiftResult(_, r.cursor), r.history))

  def resultCursorL: ShiftResult @> Option[Cursor] =
    Lens(r => Costate(ShiftResult(r.history, _), r.cursor))

  def resultCursorPL: ShiftResult @?> Cursor =
    PLensT.somePLens compose ~resultCursorL

}
