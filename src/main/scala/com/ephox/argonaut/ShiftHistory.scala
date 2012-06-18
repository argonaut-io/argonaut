package com.ephox
package argonaut

import scalaz._, Scalaz._

/**
 * A list of elements denoting the history of a cursor-shift.
 *
 * @see Shift
 * @author Tony Morris
 */
sealed trait ShiftHistory {
  /**
   * Convert to a list.
   */
  val toList: DList[ShiftHistoryElement]

  /**
   * Append two lists of cursor-shift history.
   */
  def ++(h: ShiftHistory): ShiftHistory =
    ShiftHistory.build(toList ++ h.toList)

}

object ShiftHistory extends ShiftHistorys {
  private[argonaut] def apply(e: ShiftHistoryElement) =
    build(DList(e))
}

trait ShiftHistorys {
  private[argonaut] def build(l: DList[ShiftHistoryElement]): ShiftHistory =
    new ShiftHistory {
      val toList = l
    }

  implicit val ShiftHistoryInstances: Show[ShiftHistory] with Equal[ShiftHistory] with Monoid[ShiftHistory] =
    new Show[ShiftHistory] with Equal[ShiftHistory] with Monoid[ShiftHistory] {
      def show(h: ShiftHistory) = Show[List[ShiftHistoryElement]].show(h.toList.toList)
      def equal(h1: ShiftHistory, h2: ShiftHistory) =
        h1.toList === h2.toList
      def zero = ShiftHistory.build(DList())
      def append(h1: ShiftHistory, h2: => ShiftHistory) =
        h1 ++ h2
    }
}
