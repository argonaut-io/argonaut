package com.ephox.argonaut

sealed trait ContraPossibleJson[A] {
  def copjson(a: A): PossibleJson
}

object ContraPossibleJson {
  implicit def PossibleJsonContraPossibleJson: ContraPossibleJson[PossibleJson] = new ContraPossibleJson[PossibleJson] {
    def copjson(a: PossibleJson) = a
  }

  implicit def JsonContraPossibleJson: ContraPossibleJson[Json] = new ContraPossibleJson[Json] {
    def copjson(a: Json) = PossibleJson.pJson(a)
  }

  implicit def OptionContraPossibleJson: ContraPossibleJson[Option[Json]] = new ContraPossibleJson[Option[Json]] {
    def copjson(a: Option[Json]) = a match {
      case Some(k) => PossibleJson.pJson(k)
      case None    => PossibleJson.eJson
    }
  }
}