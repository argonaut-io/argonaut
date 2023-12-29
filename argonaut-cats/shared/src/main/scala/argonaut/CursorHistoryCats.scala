package argonaut

import CursorOpCats._
import cats._
import instances.list._
import syntax.eq._

object CursorHistoryCats extends CursorHistoryCatss

trait CursorHistoryCatss {
  implicit val CursorHistoryInstances: Show[CursorHistory] & Eq[CursorHistory] & Monoid[CursorHistory] = {
    new Show[CursorHistory] with Eq[CursorHistory] with Monoid[CursorHistory] {
      override def show(h: CursorHistory) = Show[List[CursorOp]].show(h.toList)
      def eqv(h1: CursorHistory, h2: CursorHistory) = {
        h1.toList === h2.toList
      }
      def empty = CursorHistory(List())
      def combine(h1: CursorHistory, h2: CursorHistory) = {
        CursorHistory(h1.toList ::: h2.toList)
      }
    }
  }
}
