package argonaut

import monocle.Lens
import monocle.macros.GenLens

object HCursorMonocle extends HCursorMonocles

trait HCursorMonocles {
  val cursor: Lens[HCursor, Cursor] = GenLens[HCursor](_.cursor)
  val history: Lens[HCursor, CursorHistory] = GenLens[HCursor](_.history)
}
