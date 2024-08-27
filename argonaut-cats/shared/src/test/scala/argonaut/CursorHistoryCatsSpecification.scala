package argonaut

import cats.kernel.laws.discipline.MonoidTests
import arbitrary.*
import CursorHistoryCats.*
import org.typelevel.discipline.specs2.Discipline

/**
  * Created by luissanchez on 27/01/2016.
  */
class CursorHistoryCatsSpecification extends ArgonautSpec with Discipline {
  def is =
    br ^ br ^
      checkAll("CursorHistory", MonoidTests[CursorHistory].monoid)
}
