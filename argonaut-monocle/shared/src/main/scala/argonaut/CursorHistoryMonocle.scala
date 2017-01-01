package argonaut

import monocle.Iso
import monocle.macros.GenIso

object CursorHistoryMonocle extends CursorHistoryMonocles

trait CursorHistoryMonocles {
  val cursorHistory: Iso[CursorHistory, List[CursorOp]] = GenIso[CursorHistory, List[CursorOp]]
}
