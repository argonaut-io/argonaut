package argonaut

import scalaz._, Scalaz._

trait Parse {
  import Json._

  /**
   * Parses the string value and either returns a list of the failures from parsing the string
   * or an instance of the Json type if parsing succeeds.
   */
  def parse(value: String): String \/ Json

  /**
   * Parses the string value and executes one of the given functions, depending on the parse outcome.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseWith[X](value: String, success: Json => X, failure: String => X): X =
    parse(value).fold(failure, success)


  /**
   * Parses the string value and executes one of the given functions, depending on the parse outcome.
   * Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseOr[X](value: String, success: Json => X, failure: => X): X =
    parseWith(value, success, _ => failure)

  /**
   * Parses the string value to a possible JSON value.
   */
  def parseOption(value: String): Option[Json] =
    parse(value).toOption

  /**
   * Parses the string value and decodes it returning a list of all the failures stemming from
   * either the JSON parsing or the decoding.
   */
  def decode[X: DecodeJson](value: String): \/[String \/ (String, CursorHistory), X] = for {
    json <- parse(value).swapped(_.map(_.left))
    decoded <- json.jdecode[X].fold((msg, history) => (msg, history).right.left[X], _.right)
  } yield decoded

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param parsefailure Run this function if the parse produces a failure.
   * @param decodefailure Run this function if the decode produces a failure.
   */
  def decodeWith[A, X: DecodeJson](value: String, success: X => A, parsefailure: String => A, decodefailure: (String, CursorHistory) => A): A =
    decodeWithEither(value, success, _.fold(parsefailure, { case (m, h) => decodefailure(m, h) }))

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithEither[A, X: DecodeJson](value: String, success: X => A, failure: String \/ (String, CursorHistory) => A): A =
    decode(value).fold(failure, success)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithNel[A, X: DecodeJson](value: String, success: X => A, failure: String => A): A =
    decodeWith(value, success, failure, (m, h) => failure(m + ": " + h.shows))

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param default Return this value of the parse or decode fails.
   */
  def decodeOr[A, X: DecodeJson](value: String, success: X => A, default: => A): A =
    decodeWith[A, X](value, success, _ => default, (_, _) => default)

  /**
   * Parses and decodes the string value to a possible JSON value.
   */
  def decodeOption[X: DecodeJson](value: String): Option[X] =
    decode(value).toOption
}

/**
 * Library functions for parsing json.
 */
object Parse extends Parse {
  def parse(value: String): String \/ Json =
    JsonParser.parse(value)
}
