package argonaut

import scalaz._, syntax.either._, syntax.show._


/**
 * Library functions for parsing json.
 */
trait Parse[A] {
  /**
   * Parses the value and either returns a list of the failures from parsing the string
   * or an instance of the Json type if parsing succeeds.
   */
  def parse(value: A): String \/ Json

  /**
   * Parses the value and executes one of the given functions, depending on the parse outcome.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseWith[X](value: A, success: Json => X, failure: String => X): X =
    parse(value).fold(failure, success)


  /**
   * Parses the value and executes one of the given functions, depending on the parse outcome.
   * Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseOr[X](value: A, success: Json => X, failure: => X): X =
    parseWith(value, success, _ => failure)

  /**
   * Parses the value to a possible JSON value.
   */
  def parseOption(value: A): Option[Json] =
    parse(value).toOption

  /**
   * Parses the value and decodes it returning a list of all the failures stemming from
   * either the JSON parsing or the decoding.
   */
  def decode[X: DecodeJson](value: A): \/[String \/ (String, CursorHistory), X] = for {
    json <- parse(value).swapped(_.map(_.left))
    decoded <- json.jdecode[X].fold((msg, history) => (msg, history).right.left[X], _.right)
  } yield decoded

  /**
   * Parses the value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param parsefailure Run this function if the parse produces a failure.
   * @param decodefailure Run this function if the decode produces a failure.
   */
  def decodeWith[B, X: DecodeJson](value: A, success: X => B, parsefailure: String => B, decodefailure: (String, CursorHistory) => B): B =
    decodeWithEither(value, success, _.fold(parsefailure, { case (m, h) => decodefailure(m, h) }))

  /**
   * Parses the value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithEither[B, X: DecodeJson](value: A, success: X => B, failure: String \/ (String, CursorHistory) => B): B =
    decode(value).fold(failure, success)

  /**
   * Parses the value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithMessage[B, X: DecodeJson](value: A, success: X => B, failure: String => B): B =
    decodeWith(value, success, failure, (m, h) => failure(m + ": " + h.shows))

  /**
   * Parses the value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param default Return this value of the parse or decode fails.
   */
  def decodeOr[B, X: DecodeJson](value: A, success: X => B, default: => B): B =
    decodeWith[B, X](value, success, _ => default, (_, _) => default)

  /**
   * Parses and decodes the value to a possible JSON value.
   */
  def decodeOption[X: DecodeJson](value: A): Option[X] =
    decode(value).toOption

  /**
   * Parses and decodes the value to a possible JSON value.
   */
  def decodeEither[X: DecodeJson](value: A): String \/ X =
    decodeWithMessage[String \/ X, X](value, _.right[String], _.left[X])

  /**
   * Parses and decodes the value to a possible JSON value.
   */
  def decodeValidation[X: DecodeJson](value: A): Validation[String, X] =
    decodeEither(value).validation
}

/**
 * Library functions for parsing json.
 */
object Parse extends Parse[String] {

  /**
   * Parses the string value and either returns a list of the failures from parsing the string
   * or an instance of the Json type if parsing succeeds.
   */
  def parse(value: String): String \/ Json =
    JsonParser.parse(value)

}
