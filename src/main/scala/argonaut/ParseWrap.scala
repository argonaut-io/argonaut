package argonaut

import scalaz._, Scalaz._

/**
 * Utiltity for building the argonaut API over
 * various types. This is used to implement
 * StringWrap, and it is expected that it would
 * be used by integrations with other toolkits
 * to provide an argonaut API on their types.
 */
class ParseWrap[A](value: A, parser: Parse[A]) {
  /**
   * Parses the string value and either returns a list of the failures from parsing the string
   * or an instance of the Json type if parsing succeeds.
   */
  def parse: String \/ Json =
    parser.parse(value)

  /**
   * Parses the string value and executes one of the given functions, depending on the parse outcome.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseWith[X](success: Json => X, failure: String => X): X =
    parser.parseWith(value, success, failure)

  /**
   * Parses the string value and executes one of the given functions, depending on the parse outcome.
   * Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseOr[X](success: Json => X, failure: => X): X =
    parser.parseOr(value, success, failure)

  /**
   * Parses the string value to a possible JSON value.
   */
  def parseOption: Option[Json] =
    parser.parseOption(value)

  /**
   * Parses the string value and decodes it returning a list of all the failures stemming from
   * either the JSON parsing or the decoding.
   */
  def decode[X: DecodeJson]: \/[String \/ (String, CursorHistory), X] =
    parser.decode(value)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param parsefailure Run this function if the parse produces a failure.
   * @param decodefailure Run this function if the decode produces a failure.
   */
  def decodeWith[A, X: DecodeJson](success: X => A, parsefailure: String => A, decodefailure: (String, CursorHistory) => A): A =
    parser.decodeWith(value, success, parsefailure, decodefailure)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithEither[A, X: DecodeJson](success: X => A, failure: String \/ (String, CursorHistory) => A): A =
    parser.decodeWithEither(value, success, failure)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithMessage[A, X: DecodeJson](success: X => A, failure: String => A): A =
    parser.decodeWithMessage(value, success, failure)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param default Return this value of the parse or decode fails.
   */
  def decodeOr[A, X: DecodeJson](success: X => A, default: => A): A =
    parser.decodeOr(value, success, default)

  /**
   * Parses and decodes the string value to a possible JSON value.
   */
  def decodeOption[X: DecodeJson]: Option[X] =
    parser.decodeOption(value)
}
