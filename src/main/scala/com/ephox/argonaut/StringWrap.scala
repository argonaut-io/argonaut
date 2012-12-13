package com.ephox
package argonaut

import scalaz._
import Scalaz._

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
  def parse(): ValidationNEL[String, Json] = JsonParser.parse(value)

  /**
   * Parses this string value and executes one of the given functions, depending on the parse outcome.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parse[X](success: Json => X, failure: NonEmptyList[String] => X): X = {
    parse().fold(failure, success)
  }

  /**
   * Parses this string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param failure Run this function if the parse produces a failure.
   */
  def decodeParse[X: DecodeJson](failure: NonEmptyList[String] => X): DecodeResult[X] = {
    parse(_.jdecode, errors => DecodeResult(failure(errors)))
  }

  /**
   * Parses this string value and executes one of the given functions, depending on the parse outcome. Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseIgnoreError[X](success: Json => X, failure: => X) = parse(success, _ => failure)

  /**
   * Map the given function across the parsed result for a JSON value.
   */
  def parseTo[T](i: Json => T): ValidationNEL[String, T] = parse.map(i)

  /**
   * Runs the given function across a parser for a JSON value after successfully parsing. If the parser fails, then this
   * function will fail. ''WARNING: Partial Function''
   */
  def parseToOrDie[T](i: Json => T): T = {
    parseTo(i).fold[T](
      errors => sys.error("Unsuccessful parse result: " + errors.shows), 
      identity
    )
  }

  /**
   * Parses this string value to a possible JSON value.
   */
  def pparse: Option[Json] = parseIgnoreError(Some(_), None)

  /*
   * Construct a pair of the key and JSON value.
   */
  def :=[A: EncodeJson](a: A) =
    (value, implicitly[EncodeJson[A]].apply(a))

  /*
   * Construct a pair of the key and JSON value.
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
