package argonaut

import Json._
import scalaz._

object CursorOpElementScalaz extends CursorOpElementScalazs

trait CursorOpElementScalazs {
  implicit val CursorOpElementInstances: Show[CursorOpElement] with Equal[CursorOpElement] = {
    new Show[CursorOpElement] with Equal[CursorOpElement] {
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

      def equal(e1: CursorOpElement, e2: CursorOpElement) = {
        e1 == e2
      }
    }
  }

  def cursorOpLeftNL: CursorOpElement @?> Int =
    PLens {
      case CursorOpLeftN(n) => Some(Store(CursorOpLeftN(_), n))
      case _ => None
    }

  def cursorOpRightNL: CursorOpElement @?> Int =
    PLens {
      case CursorOpRightN(n) => Some(Store(CursorOpRightN(_), n))
      case _ => None
    }

  def cursorOpLeftAtL: CursorOpElement @?> (Json => Boolean) =
    PLens {
      case CursorOpLeftAt(p) => Some(Store(CursorOpLeftAt(_), p))
      case _ => None
    }

  def cursorOpRightAtL: CursorOpElement @?> (Json => Boolean) =
    PLens {
      case CursorOpRightAt(p) => Some(Store(CursorOpRightAt(_), p))
      case _ => None
    }

  def cursorOpFindL: CursorOpElement @?> (Json => Boolean) =
    PLens {
      case CursorOpFind(p) => Some(Store(CursorOpFind(_), p))
      case _ => None
    }

  def cursorOpFieldL: CursorOpElement @?> JsonField =
    PLens {
      case CursorOpField(f) => Some(Store(CursorOpField(_), f))
      case _ => None
    }

  def cursorOpDownFieldL: CursorOpElement @?> JsonField =
    PLens {
      case CursorOpDownField(f) => Some(Store(CursorOpDownField(_), f))
      case _ => None
    }

  def cursorOpDownAtL: CursorOpElement @?> (Json => Boolean) =
    PLens {
      case CursorOpDownAt(p) => Some(Store(CursorOpDownAt(_), p))
      case _ => None
    }

  def cursorOpDownNL: CursorOpElement @?> Int =
    PLens {
      case CursorOpDownN(n) => Some(Store(CursorOpDownN(_), n))
      case _ => None
    }

  def cursorOpDeleteGoFieldL: CursorOpElement @?> JsonField =
    PLens {
      case CursorOpDeleteGoField(f) => Some(Store(CursorOpDeleteGoField(_), f))
      case _ => None
    }
}
