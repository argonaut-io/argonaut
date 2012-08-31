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
   * Convert cursor history operations to a list (O(n)).
   */
  val toList: List[CursorOp]

  def head: Option[CursorOp] =
    toList.headOption

  /**
   * Append two lists of cursor history.
   */
  def ++(h: CursorHistory): CursorHistory  =
    CursorHistory.build(toList ++ h.toList)

  /**
   * Prepend a cursor operation to the history.
   */
  def +:(o: CursorOp): CursorHistory =
    CursorHistory.build(o +: toList)

  def failedACursor(c: Cursor): ACursor =
    ACursor.failedACursor(HCursor(c, this))

  def acursor(c: Cursor): ACursor =
    ACursor(HCursor(c, this))

  def acursorElement(c: Store[Cursor, Option[Cursor]], e: CursorOpElement): ACursor = {
    val x = c.pos
    c.copoint match {
      case None => +:(CursorOp(e)).failedACursor(x)
      case Some(q) => +:(CursorOp.failedOp(e)).acursor(q)
    }
  }

}

object CursorHistory extends CursorHistorys {
  private[argonaut] def apply(e: CursorOp) =
    build(List(e))
}

trait CursorHistorys {
  private[argonaut] def build(l: List[CursorOp]): CursorHistory =
    new CursorHistory {
      val toList = l
    }

  implicit val CursorHistoryInstances: Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] =
    new Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] {
      override def show(h: CursorHistory) = Show[List[CursorOp]].show(h.toList)
      def equal(h1: CursorHistory, h2: CursorHistory) =
        h1.toList === h2.toList
      def zero = CursorHistory.build(List())
      def append(h1: CursorHistory, h2: => CursorHistory) =
        CursorHistory.build(h1.toList ::: h2.toList)
    }
}