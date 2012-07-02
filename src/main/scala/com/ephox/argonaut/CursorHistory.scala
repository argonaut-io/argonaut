package com.ephox
package argonaut

import scalaz._, Scalaz._

/**
 * A list of elements denoting the history of a cursor.
 *
 * @see Shift
 * @author Tony Morris
 */
sealed trait CursorHistory {
  /**
   * Convert to a list.
   */
  val ops: DList[CursorOp]

  /**
   * Convert cursor history operations to a list (O(n)).
   */
  def toList: List[CursorOp] =
    ops.toList

  /**
   * Append two lists of cursor history.
   */
  def ++(h: CursorHistory): CursorHistory  =
    CursorHistory.build(ops ++ h.ops)

  /**
   * Prepend a cursor operation to the history.
   */
  def +:(o: CursorOp): CursorHistory =
    CursorHistory.build(o +: ops)

  /**
   * Append a cursor operation to the history.
   */
  def :+(o: CursorOp): CursorHistory =
    CursorHistory.build(ops :+ o)

  def failedACursor(c: Cursor): ACursor =
    ACursor.failedACursor(HCursor(c, this))

  def acursor(c: Cursor): ACursor =
    ACursor(HCursor(c, this))

  def acursorElement(c: Store[Cursor, Option[Cursor]], e: CursorOpElement): ACursor = {
    val x = c.pos
    c.copoint match {
      case None => :+(CursorOp(e)).failedACursor(x)
      case Some(q) => :+(CursorOp.failedOp(e)).acursor(q)
    }
  }

}

object CursorHistory extends CursorHistorys {
  private[argonaut] def apply(e: CursorOp) =
    build(DList(e))
}

trait CursorHistorys {
  private[argonaut] def build(l: DList[CursorOp]): CursorHistory =
    new CursorHistory {
      val ops = l
    }

  implicit val CursorHistoryInstances: Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] =
    new Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] {
      def show(h: CursorHistory) = Show[List[CursorOp]].show(h.ops.toList)
      def equal(h1: CursorHistory, h2: CursorHistory) =
        h1.ops === h2.ops
      def zero = CursorHistory.build(DList())
      def append(h1: CursorHistory, h2: => CursorHistory) =
        h1 ++ h2
    }
}