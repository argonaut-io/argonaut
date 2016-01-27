package argonaut

import cats._

object PrettyParamsCats extends PrettyParamsCatss

trait PrettyParamsCatss {
  implicit val PrettyParamsEq: Eq[PrettyParams] = Eq.fromUniversalEquals
}
