package argonaut

import scalaz._, Scalaz._

/**
 * A list of elements denoting the history of a cursor.
 *
 * @author Tony Morris
 */
case class CursorHistory(toList: List[CursorOp]) {
  def head: Option[CursorOp] =
    toList.headOption

  /**
   * Append two lists of cursor history.
   */
  def ++(h: CursorHistory): CursorHistory  =
    CursorHistory(toList ++ h.toList)

  /**
   * Prepend a cursor operation to the history.
   */
  def +:(o: CursorOp): CursorHistory =
    CursorHistory(o +: toList)

  def failedACursor(c: Cursor): ACursor =
    ACursor.fail(HCursor(c, this))

  def acursor(c: Cursor): ACursor =
    ACursor.ok(HCursor(c, this))

  def acursorElement(c: Store[Cursor, Option[Cursor]], e: CursorOpElement): ACursor = {
    val x = c.pos
    c.copoint match {
      case None => +:(CursorOp.failedOp(e)).failedACursor(x)
      case Some(q) => +:(CursorOp(e)).acursor(q)
    }
  }

}

object CursorHistory extends CursorHistorys

trait CursorHistorys {
  def start(e: CursorOp) =
    CursorHistory(List(e))

  implicit val CursorHistoryInstances: Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] =
    new Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] {
      override def show(h: CursorHistory) = Show[List[CursorOp]].show(h.toList)
      def equal(h1: CursorHistory, h2: CursorHistory) =
        h1.toList === h2.toList
      def zero = CursorHistory(List())
      def append(h1: CursorHistory, h2: => CursorHistory) =
        CursorHistory(h1.toList ::: h2.toList)
    }
}
