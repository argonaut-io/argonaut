package com.ephox.argonaut

/**
 * A contravariant function to [[com.ephox.argonaut.PossibleJson]].
 *
 * @author Tony Morris
 */
sealed trait ContraPossibleJson[A] {
  /**
   * Convert the given value to a possible JSON value.
   */
  def copjson(a: A): PossibleJson
}

/**
 * Constructors and other utilities for contravariant possible JSON values.
 *
 * @author Tony Morris
 */
object ContraPossibleJson {
  /**
   * Identity.
   */
  implicit def PossibleJsonContraPossibleJson: ContraPossibleJson[PossibleJson] = new ContraPossibleJson[PossibleJson] {
    def copjson(a: PossibleJson) = a
  }

  /**
   * Convert a JSON value to a possible JSON value.
   */
  implicit def JsonContraPossibleJson: ContraPossibleJson[Json] = new ContraPossibleJson[Json] {
    def copjson(a: Json) = PossibleJson.pJson(a)
  }

  /**
   * Convert an `Option[Json]` to a possible JSON value.
   */
  implicit def OptionContraPossibleJson: ContraPossibleJson[Option[Json]] = new ContraPossibleJson[Option[Json]] {
    def copjson(a: Option[Json]) = a match {
      case Some(k) => PossibleJson.pJson(k)
      case None    => PossibleJson.eJson
    }
  }
}