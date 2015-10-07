package argonaut

import monocle.macros.GenLens
import scalaz._
import scala.annotation._

object PrettyParamsMonocle extends PrettyParamsMonocles

trait PrettyParamsMonocles {
  val lenser = GenLens[PrettyParams]

  val indentL = lenser(_.indent)
  val lbraceLeftL = lenser(_.lbraceLeft)
  val lbraceRightL = lenser(_.lbraceRight)
  val rbraceLeftL = lenser(_.rbraceLeft)
  val rbraceRightL = lenser(_.rbraceRight)
  val lbracketLeftL = lenser(_.lbracketLeft)
  val lbracketRightL = lenser(_.lbracketRight)
  val rbracketLeftL = lenser(_.rbracketLeft)
  val rbracketRightL = lenser(_.rbracketRight)
  val lrbracketsEmptyL = lenser(_.lrbracketsEmpty)
  val arrayCommaLeftL = lenser(_.arrayCommaLeft)
  val arrayCommaRightL = lenser(_.arrayCommaRight)
  val objectCommaLeftL = lenser(_.objectCommaLeft)
  val objectCommaRightL = lenser(_.objectCommaRight)
  val colonLeftL = lenser(_.colonLeft)
  val colonRightL = lenser(_.colonRight)
  val preserveOrderL = lenser(_.preserveOrder)
  val dropNullKeysL = lenser(_.dropNullKeys)
}
