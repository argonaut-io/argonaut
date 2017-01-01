package argonaut

import scalaz._

object PrettyParamsScalaz extends PrettyParamsScalazs

trait PrettyParamsScalazs {
  implicit val prettyParamsEqual: Equal[PrettyParams] = Equal.equalA
}
