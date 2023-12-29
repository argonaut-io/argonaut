package argonaut

import scalaz._
import syntax.show._
import CursorOpElementScalaz._

object CursorOpScalaz extends CursorOpScalazs {}

trait CursorOpScalazs {
  implicit val CursorOpInstances: Show[CursorOp] & Equal[CursorOp] = {
    new Show[CursorOp] with Equal[CursorOp] {
      override def show(x: CursorOp) = x match {
        case Reattempt =>
          Cord(".?.")
        case El(o, s) =>
          if (s) o.show else Cord("*." + o.shows)
      }
      def equal(a1: CursorOp, a2: CursorOp) = {
        a1 == a2
      }
    }
  }
}
