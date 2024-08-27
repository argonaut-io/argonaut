package argonaut

import scalaz.*

object PrettyParamsScalaz extends PrettyParamsScalazs

trait PrettyParamsScalazs {
  implicit val prettyParamsEqual: Equal[PrettyParams] = Equal.equalA
}
