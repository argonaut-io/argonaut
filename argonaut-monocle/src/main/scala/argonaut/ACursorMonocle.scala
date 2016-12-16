package argonaut

import monocle.macros.GenIso
import monocle.{Iso, Prism}
import monocle.std.either


object ACursorMonocle extends ACursorMonocles

trait ACursorMonocles {
  val aCursor: Iso[ACursor, Either[HCursor, HCursor]] = GenIso[ACursor, Either[HCursor, HCursor]]
  val hSuccess: Prism[ACursor, HCursor] = aCursor composePrism either.stdRight
  val hFail: Prism[ACursor, HCursor]    = aCursor composePrism either.stdLeft
}
