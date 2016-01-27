package argonaut

import cats._

object CursorOpElementCats extends CursorOpElementCatss

trait CursorOpElementCatss {
  implicit val CursorOpElementInstances: Show[CursorOpElement] with Eq[CursorOpElement] = {
    new Show[CursorOpElement] with Eq[CursorOpElement] {
      override def show(e: CursorOpElement) = {
        e match {
          case CursorOpLeft => "<-"
          case CursorOpRight => "->"
          case CursorOpFirst => "|<-"
          case CursorOpLast => "->|"
          case CursorOpUp => "_/"
          case CursorOpLeftN(n) => "-<-:(" + n + ")"
          case CursorOpRightN(n) => ":->-(" + n + ")"
          case CursorOpLeftAt(_) => "?<-:"
          case CursorOpRightAt(_) => ":->?"
          case CursorOpFind(_) => "find"
          case CursorOpField(f) => "--(" + f + ")"
          case CursorOpDownField(f) => "--\\(" + f + ")"
          case CursorOpDownArray => "\\\\"
          case CursorOpDownAt(_) => "-\\"
          case CursorOpDownN(n) => "=\\(" + n + ")"
          case CursorOpDeleteGoParent => "!_/"
          case CursorOpDeleteGoLeft => "<-!"
          case CursorOpDeleteGoRight => "!->"
          case CursorOpDeleteGoFirst => "|<-!"
          case CursorOpDeleteGoLast => "!->|"
          case CursorOpDeleteGoField(f) => "!--(" + f + ")"
          case CursorOpDeleteLefts => "!<"
          case CursorOpDeleteRights => ">!"
          case CursorOpSetLefts(_) => "!...<"
          case CursorOpSetRights(_) => ">...!"
        }
      }

      def eqv(e1: CursorOpElement, e2: CursorOpElement) = {
        e1 == e2
      }
    }
  }
}
