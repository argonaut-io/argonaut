package argonaut

import scalaz._, Scalaz._

sealed abstract class CursorOp extends Product with Serializable {
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

case object Reattempt extends CursorOp
case class El(o: CursorOpElement, success: Boolean) extends CursorOp

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
      override def show(x: CursorOp) = x match {
        case Reattempt => ".?."
        case El(o, s) => if(s) o.show else '*' -: '.' -: o.show
      }
      def equal(a1: CursorOp, a2: CursorOp) =
        a1 == a2
    }

}
