package argonaut

/**
 * A list of elements denoting the history of a cursor.
 *
 * Note: Most recent operation appears at head of list.
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

  override def toString(): String = "CursorHistory(%s)".format(toList.toString)
}

object CursorHistory extends CursorHistorys

trait CursorHistorys {
  def empty = CursorHistory(List())

  def start(e: CursorOp) = CursorHistory(List(e))
}
