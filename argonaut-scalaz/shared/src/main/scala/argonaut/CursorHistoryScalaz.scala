package argonaut

import scalaz.*
import std.list.*
import syntax.equal.*
import CursorOpScalaz.*

object CursorHistoryScalaz extends CursorHistoryScalazs

trait CursorHistoryScalazs {
  implicit val CursorHistoryInstances: Show[CursorHistory] & Equal[CursorHistory] & Monoid[CursorHistory] = {
    new Show[CursorHistory] with Equal[CursorHistory] with Monoid[CursorHistory] {
      override def show(h: CursorHistory) = Show[List[CursorOp]].show(h.toList)
      def equal(h1: CursorHistory, h2: CursorHistory) = {
        h1.toList === h2.toList
      }
      def zero = CursorHistory(List())
      def append(h1: CursorHistory, h2: => CursorHistory) = {
        CursorHistory(h1.toList ::: h2.toList)
      }
    }
  }
}
