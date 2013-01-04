package argonaut

import scalaz._, Scalaz._

/**
 * Wraps a `String` value and provides methods, particularly for parsing.
 *
 * @author Tony Morris
 */
sealed trait StringWrap {
  /**
   * The wrapped value.
   */
  val value: String

  import Json._

  /**
   * Parses the string value and either returns a list of the failures from parsing the string
   * or an instance of the Json type if parsing succeeds.
   */
  def parse: String \/ Json =
    Parse.parse(value)

  /**
   * Parses the string value and executes one of the given functions, depending on the parse outcome.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseWith[X](success: Json => X, failure: String => X): X =
    Parse.parseWith(value, success, failure)

  /**
   * Parses the string value and executes one of the given functions, depending on the parse outcome.
   * Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseOr[X](success: Json => X, failure: => X): X =
    Parse.parseOr(value, success, failure)

  /**
   * Parses the string value to a possible JSON value.
   */
  def parseOption: Option[Json] =
    Parse.parseOption(value)

  /**
   * Parses the string value and decodes it returning a list of all the failures stemming from
   * either the JSON parsing or the decoding.
   */
  def decode[X: DecodeJson]: \/[String \/ (String, CursorHistory), X] =
    Parse.decode(value)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param parsefailure Run this function if the parse produces a failure.
   * @param decodefailure Run this function if the decode produces a failure.
   */
  def decodeWith[A, X: DecodeJson](success: X => A, parsefailure: String => A, decodefailure: (String, CursorHistory) => A): A =
    Parse.decodeWith(value, success, parsefailure, decodefailure)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithEither[A, X: DecodeJson](success: X => A, failure: String \/ (String, CursorHistory) => A): A =
    Parse.decodeWithEither(value, success, failure)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeWithNel[A, X: DecodeJson](success: X => A, failure: String => A): A =
    Parse.decodeWithNel(value, success, failure)

  /**
   * Parses the string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param default Return this value of the parse or decode fails.
   */
  def decodeOr[A, X: DecodeJson](success: X => A, default: => A): A =
    Parse.decodeOr(value, success, default)

  /**
   * Parses and decodes the string value to a possible JSON value.
   */
  def decodeOption[X: DecodeJson]: Option[X] =
    Parse.decodeOption(value)

  /*
   * Construct a pair of the key and JSON value.
   *
   * Example:
   * {{{
   *   ("key1" := "value1") ->: ("key2" := "value2") ->: jEmptyObject
   * }}}
   */
  def :=[A: EncodeJson](a: A) =
    (value, implicitly[EncodeJson[A]].apply(a))

  /*
   * Construct an optional pair of the key and JSON value.
   *
   * Example:
   * {{{
   *   ("key1" :?= None) ->?: ("key2" :=? Some("value2")) ->?: jEmptyObject
   * }}}
   */
  def :=?[A: EncodeJson](a: Option[A]) =
    a map (aa =>  (value, implicitly[EncodeJson[A]].apply(aa)))

}

object StringWrap extends StringWraps

/**
 * Constructors and other utilities for wrapped string values.
 *
 * @author Tony Morris
 */
trait StringWraps {
  /**
   * Implicitly wraps the given string value.
   */
  implicit def StringStringWrap(s: String): StringWrap = new StringWrap {
    val value = s
  }
}
