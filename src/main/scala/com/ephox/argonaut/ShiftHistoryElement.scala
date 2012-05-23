package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

sealed trait ShiftHistoryElement {

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
}
