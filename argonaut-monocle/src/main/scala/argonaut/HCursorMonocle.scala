package argonaut

import monocle.macros._

object HCursorMonocle extends HCursorMonocles

trait HCursorMonocles {
 val hCursorCursorL = GenLens[HCursor](_.cursor)
 val hCursorCursorHistoryL = GenLens[HCursor](_.history)
}
