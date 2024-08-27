package argonaut

import cats.*

object PrettyParamsCats extends PrettyParamsCatss

trait PrettyParamsCatss {
  implicit val PrettyParamsEq: Eq[PrettyParams] = Eq.fromUniversalEquals
}
