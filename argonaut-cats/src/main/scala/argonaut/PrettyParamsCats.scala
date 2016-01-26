package argonaut

import cats._

object PrettyParamsCats extends PrettyParamsCatss

trait PrettyParamsCatss {
  implicit val prettyParamsEq: Eq[PrettyParams] = Eq.fromUniversalEquals
}
