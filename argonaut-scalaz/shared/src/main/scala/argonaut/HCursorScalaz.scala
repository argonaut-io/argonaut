package argonaut

import scalaz.*

object HCursorScalaz extends HCursorScalazs

trait HCursorScalazs {
  val hcursorL: HCursor @> Cursor =
    Lens(c => Store(HCursor(_, c.history), c.cursor))

  val hcursorHistoryL: HCursor @> CursorHistory =
    Lens(c => Store(HCursor(c.cursor, _), c.history))
}
