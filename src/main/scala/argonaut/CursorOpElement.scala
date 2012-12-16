package argonaut

import Json._
import scalaz._, Scalaz._

sealed trait CursorOpElement {
  def isLeft: Boolean =
    this == CursorOpLeft

  def isRight: Boolean =
    this == CursorOpRight

  def isFirst: Boolean =
    this == CursorOpFirst

  def isLast: Boolean =
    this == CursorOpLast

  def isUp: Boolean =
    this == CursorOpUp

  def isDeleteGoParent: Boolean =
    this == CursorOpDeleteGoParent

  def isDeleteGoLeft: Boolean =
    this == CursorOpDeleteGoLeft

  def isDeleteGoRight: Boolean =
    this == CursorOpDeleteGoRight

  def isDeleteGoFirst: Boolean =
    this == CursorOpDeleteGoFirst

  def isDeleteGoLast: Boolean =
    this == CursorOpDeleteGoLast

  def isDeleteLefts: Boolean =
    this == CursorOpDeleteLefts

  def isDeleteRights: Boolean =
    this == CursorOpDeleteRights

}
case object CursorOpLeft extends CursorOpElement
case object CursorOpRight extends CursorOpElement
case object CursorOpFirst extends CursorOpElement
case object CursorOpLast extends CursorOpElement
case object CursorOpUp extends CursorOpElement
case class CursorOpLeftN(n: Int) extends CursorOpElement
case class CursorOpRightN(n: Int) extends CursorOpElement
case class CursorOpLeftAt(p: Json => Boolean) extends CursorOpElement
case class CursorOpRightAt(p: Json => Boolean) extends CursorOpElement
case class CursorOpField(f: JsonField) extends CursorOpElement
case class CursorOpDownField(f: JsonField) extends CursorOpElement
case object CursorOpDownArray extends CursorOpElement
case class CursorOpDownAt(p: Json => Boolean) extends CursorOpElement
case class CursorOpDownN(n: Int) extends CursorOpElement
case object CursorOpDeleteGoParent extends CursorOpElement
case object CursorOpDeleteGoLeft extends CursorOpElement
case object CursorOpDeleteGoRight extends CursorOpElement
case object CursorOpDeleteGoFirst extends CursorOpElement
case object CursorOpDeleteGoLast extends CursorOpElement
case class CursorOpDeleteGoField(f: JsonField) extends CursorOpElement
case object CursorOpDeleteLefts extends CursorOpElement
case object CursorOpDeleteRights extends CursorOpElement

object CursorOpElement extends CursorOpElements

trait CursorOpElements {
  implicit val CursorOpElementInstances: Show[CursorOpElement] with Equal[CursorOpElement] =
    new Show[CursorOpElement] with Equal[CursorOpElement] {
      override def show(e: CursorOpElement) =
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
        }

      def equal(e1: CursorOpElement, e2: CursorOpElement) =
        e1 == e2
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
