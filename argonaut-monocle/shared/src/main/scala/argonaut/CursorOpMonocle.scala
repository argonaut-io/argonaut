package argonaut

import monocle.Prism
import monocle.macros.GenPrism
import monocle.macros.GenIso

object CursorOpMonocle extends CursorOpMonocles

trait CursorOpMonocles {
  val reattempt: Prism[CursorOp, Unit] = GenPrism[CursorOp, Reattempt.type] andThen GenIso.unit[Reattempt.type]
  val el: Prism[CursorOp, (CursorOpElement, Boolean)] = Prism[CursorOp, (CursorOpElement, Boolean)] {
    case Reattempt => None
    case El(op, success) => Some((op, success))
  } { case (op, success) => El(op, success) }
}
