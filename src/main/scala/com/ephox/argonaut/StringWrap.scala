package com.ephox
package argonaut


import util.parsing.input.CharSequenceReader

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

  import PossibleJson._

  /**
   * Parses this string value and executes one of the given functions, depending on the parse outcome. To understand the
   * distinction between an `error` and a `failure` consult the Scala parser combinator API.
   *
   * @param success Run this function if the parse succeeds.
   * @param error Run this function if the parse produces an error.
   * @param failure Run this function if the parse produces a failure.
   */
  def parse[X](success: Json => X, error: String => X, failure: String => X) = {
    val p = new JsonParser
    val r = new CharSequenceReader(value)
    p.jvalue(r) match {
      case p.Success(j, _) => success(j)
      case p.Error(e, _) => sys.error("Could not parse json, error [" + e + "] in [\n" + value + "\n]")
      case p.Failure(e, _) => failure("Could not parse json, failure [" + e + "] in [\n" + value + "\n]")
    }
  }

  /**
   * Parses this string value and executes one of the given functions, depending on the parse outcome. The distinction
   * between a parse `error` and a `failure` is not made by this function.
   *
   * @param success Run this function if the parse succeeds.
   * @param nosuccess Run this function if the parse produces an error or a failure.
   */
  def parseIgnoreErrorType[X](success: Json => X, nosuccess: String => X) =
    parse(success, nosuccess, nosuccess)

  /**
   * Parses this string value and executes one of the given functions, depending on the parse outcome. The distinction
   * between a parse `error` and a `failure` is not made by this function. Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param nosuccess Run this function if the parse produces an error or a failure.
   */
  def parseIgnoreError[X](success: Json => X, nosuccess: => X) =
    parseIgnoreErrorType(success, _ => nosuccess)

  /**
   * A parser for this string value to a JSON value.
   */
  def parse = {
    val p = new JsonParser
    val r = new CharSequenceReader(value)
    p.jvalue(r)
  }

  /**
   * Map the given function across a parser for a JSON value.
   */
  def parseTo[T](i: Json => T) = parse.map(i(_))

  /**
   * Runs the given function across a parser for a JSON value after successfully parsing. If the parser fails, then this
   * function will fail. ''WARNING: Partial Function''
   */
  def parseToOrDie[T](i: Json => T): T = {
    val r = parseTo(i)
    if (r.successful) r.get else error("Unsuccessful parse result: " + r)
  }

  /**
   * Parses this string value to a possible JSON value.
   */
  def pparse: PossibleJson = parseIgnoreError(pJson, eJson)
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
