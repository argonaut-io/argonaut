package argonaut

import CursorOpElementCats._
import cats._
import syntax.show._

object CursorOpCats extends CursorOpCatss {
}

trait CursorOpCatss {
  implicit val CursorOpInstances: Show[CursorOp] with Eq[CursorOp] = {
    new Show[CursorOp] with Eq[CursorOp] {
      override def show(x: CursorOp) = x match {
        case Reattempt => ".?."
        case El(o, s) => if (s) o.show else '*' +: '.' +: o.show
      }
      def eqv(a1: CursorOp, a2: CursorOp) = {
        a1 == a2
      }
    }
  }
}
