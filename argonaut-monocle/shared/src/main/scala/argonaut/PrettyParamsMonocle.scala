package argonaut

import monocle.Lens
import monocle.macros.GenLens

object PrettyParamsMonocle extends PrettyParamsMonocles

trait PrettyParamsMonocles {

  val indent: Lens[PrettyParams, String] = GenLens[PrettyParams](_.indent)
  val lbraceLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.lbraceLeft)
  val lbraceRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.lbraceRight)
  val rbraceLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.rbraceLeft)
  val rbraceRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.rbraceRight)
  val lbracketLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.lbracketLeft)
  val lbracketRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.lbracketRight)
  val rbracketLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.rbracketLeft)
  val rbracketRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.rbracketRight)
  val lrbracketsEmpty: Lens[PrettyParams, String] = GenLens[PrettyParams](_.lrbracketsEmpty)
  val arrayCommaLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.arrayCommaLeft)
  val arrayCommaRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.arrayCommaRight)
  val objectCommaLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.objectCommaLeft)
  val objectCommaRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.objectCommaRight)
  val colonLeft: Lens[PrettyParams, String] = GenLens[PrettyParams](_.colonLeft)
  val colonRight: Lens[PrettyParams, String] = GenLens[PrettyParams](_.colonRight)
  val preserveOrder: Lens[PrettyParams, Boolean] = GenLens[PrettyParams](_.preserveOrder)
  val dropNullKeys: Lens[PrettyParams, Boolean] = GenLens[PrettyParams](_.dropNullKeys)
}
