package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

sealed trait ShiftHistoryElement {
  def isLeft: Boolean =
    this == ShiftLeft

  def isRight: Boolean =
    this == ShiftRight

  def isFirst: Boolean =
    this == ShiftFirst

  def isLast: Boolean =
    this == ShiftLast

  def isUp: Boolean =
    this == ShiftUp

  def isDeleteGoParent: Boolean =
    this == ShiftDeleteGoParent

  def isDeleteGoLeft: Boolean =
    this == ShiftDeleteGoLeft

  def isDeleteGoRight: Boolean =
    this == ShiftDeleteGoRight

  def isDeleteGoFirst: Boolean =
    this == ShiftDeleteGoFirst

  def isDeleteGoLast: Boolean =
    this == ShiftDeleteGoLast
}
case object ShiftLeft extends ShiftHistoryElement
case object ShiftRight extends ShiftHistoryElement
case object ShiftFirst extends ShiftHistoryElement
case object ShiftLast extends ShiftHistoryElement
case object ShiftUp extends ShiftHistoryElement
case class ShiftLeftN(n: Int) extends ShiftHistoryElement
case class ShiftRightN(n: Int) extends ShiftHistoryElement
case class ShiftLeftAt(p: Json => Boolean) extends ShiftHistoryElement
case class ShiftRightAt(p: Json => Boolean) extends ShiftHistoryElement
case class ShiftField(f: JsonField) extends ShiftHistoryElement
case class ShiftDownField(f: JsonField) extends ShiftHistoryElement
case object ShiftDownArray extends ShiftHistoryElement
case class ShiftDownAt(p: Json => Boolean) extends ShiftHistoryElement
case class ShiftDownN(n: Int) extends ShiftHistoryElement
case object ShiftDeleteGoParent extends ShiftHistoryElement
case object ShiftDeleteGoLeft extends ShiftHistoryElement
case object ShiftDeleteGoRight extends ShiftHistoryElement
case object ShiftDeleteGoFirst extends ShiftHistoryElement
case object ShiftDeleteGoLast extends ShiftHistoryElement
case class ShiftDeleteGoField(f: JsonField) extends ShiftHistoryElement
case class ShiftCustom(m: String) extends ShiftHistoryElement

object ShiftHistoryElement extends ShiftHistoryElements

trait ShiftHistoryElements {
  implicit val ShiftHistoryElementInstances: Show[ShiftHistoryElement] with Equal[ShiftHistoryElement] =
    new Show[ShiftHistoryElement] with Equal[ShiftHistoryElement] {
      def show(e: ShiftHistoryElement) =
        (e match {
          case ShiftLeft => "<-"
          case ShiftRight => "->"
          case ShiftFirst => "|<-"
          case ShiftLast => "->|"
          case ShiftUp => "_/"
          case ShiftLeftN(n) => "-<-:(" + n + ")"
          case ShiftRightN(n) => ":->-(" + n + ")"
          case ShiftLeftAt(_) => "?<-:"
          case ShiftRightAt(_) => ":->?"
          case ShiftField(f) => "--(" + f + ")"
          case ShiftDownField(f) => "--\\(" + f + ")"
          case ShiftDownArray => "\\\\"
          case ShiftDownAt(_) => "-\\"
          case ShiftDownN(n) => "=\\(" + n + ")"
          case ShiftDeleteGoParent => "!_/"
          case ShiftDeleteGoLeft => "<-!"
          case ShiftDeleteGoRight => "!->"
          case ShiftDeleteGoFirst => "|<-!"
          case ShiftDeleteGoLast => "!->|"
          case ShiftDeleteGoField(f) => "!--(" + f + ")"
          case ShiftCustom(m) => "{" + m + "}"
        }).toList

      def equal(e1: ShiftHistoryElement, e2: ShiftHistoryElement) =
        e1 == e2
    }

  def shiftLeftNL: ShiftHistoryElement @?> Int =
    PLens {
      case ShiftLeftN(n) => Some(Store(ShiftLeftN(_), n))
      case _ => None
    }

  def shiftRightNL: ShiftHistoryElement @?> Int =
    PLens {
      case ShiftRightN(n) => Some(Store(ShiftRightN(_), n))
      case _ => None
    }

  def shiftLeftAtL: ShiftHistoryElement @?> (Json => Boolean) =
    PLens {
      case ShiftLeftAt(p) => Some(Store(ShiftLeftAt(_), p))
      case _ => None
    }

  def shiftRightAtL: ShiftHistoryElement @?> (Json => Boolean) =
    PLens {
      case ShiftRightAt(p) => Some(Store(ShiftRightAt(_), p))
      case _ => None
    }

  def shiftFieldL: ShiftHistoryElement @?> JsonField =
    PLens {
      case ShiftField(f) => Some(Store(ShiftField(_), f))
      case _ => None
    }

  def shiftDownFieldL: ShiftHistoryElement @?> JsonField =
    PLens {
      case ShiftDownField(f) => Some(Store(ShiftDownField(_), f))
      case _ => None
    }

  def shiftDownAtL: ShiftHistoryElement @?> (Json => Boolean) =
    PLens {
      case ShiftDownAt(p) => Some(Store(ShiftDownAt(_), p))
      case _ => None
    }

  def shiftDownNL: ShiftHistoryElement @?> Int =
    PLens {
      case ShiftDownN(n) => Some(Store(ShiftDownN(_), n))
      case _ => None
    }

  def shiftDeleteGoFieldL: ShiftHistoryElement @?> JsonField =
    PLens {
      case ShiftDeleteGoField(f) => Some(Store(ShiftDeleteGoField(_), f))
      case _ => None
    }

  def shiftCustomL: ShiftHistoryElement @?> String =
    PLens {
      case ShiftCustom(s) => Some(Store(ShiftCustom(_), s))
      case _ => None
    }


}
