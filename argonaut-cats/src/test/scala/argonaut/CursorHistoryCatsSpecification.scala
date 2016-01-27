package argonaut

import algebra.laws.GroupLaws
import arbitrary._
import CursorHistoryCats._
import org.specs2.Specification
import org.typelevel.discipline.specs2.Discipline

/**
  * Created by luissanchez on 27/01/2016.
  */
class CursorHistoryCatsSpecification extends Specification with Discipline { def is =
  br ^ br ^
  checkAll("CursorHistory", GroupLaws[CursorHistory].monoid)
}
