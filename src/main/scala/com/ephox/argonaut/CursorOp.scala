package com.ephox
package argonaut

import scalaz._, Scalaz._

sealed trait CursorOp {
  def isReattempt: Boolean =
    this == Reattempt

  def isNotReattempt: Boolean =
    this != Reattempt

  def succeeded: Boolean =
    this match {
      case Reattempt => false
      case El(_, s) => s
    }

  def failed: Boolean =
    this match {
      case Reattempt => false
      case El(_, s) => !s
    }
}
private case object Reattempt extends CursorOp
private case class El(o: CursorOpElement, success: Boolean) extends CursorOp

object CursorOp extends CursorOps {
  def apply(o: CursorOpElement): CursorOp =
    El(o, true)
}

trait CursorOps {
  def reattemptOp: CursorOp =
    Reattempt

  def failedOp(o: CursorOpElement): CursorOp =
      El(o, false)

  implicit val CursorOpInstances: Show[CursorOp] with Equal[CursorOp] =
    new Show[CursorOp] with Equal[CursorOp] {
      def show(x: CursorOp) = x match {
        case Reattempt => List('.', '?', '.')
        case El(o, s) => if(s) o.show else '*' :: '.' :: o.show
      }
      def equal(a1: CursorOp, a2: CursorOp) =
        a1 == a2
    }

}