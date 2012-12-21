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
   * Parses the string value and decodes it returning a list of all the failures stemming from
   * either the JSON parsing or the decoding.
   */
  def parseDecode[X: DecodeJson](): ValidationNEL[String, X] = for {
    json <- parse()
    decoded <- json.jdecode[X].result.fold(failure => ("Failure decoding JSON: " + failure._2.shows).failNel[X], _.successNel)
  } yield decoded

  /**
   * Parses this string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param parsefailure Run this function if the parse produces a failure.
   * @param decodefailure Run this function if the decode produces a failure.
   */
  def parseDecodeWith[A, X: DecodeJson](success: X => A, parsefailure: NonEmptyList[String] => A, decodefailure: (String, CursorHistory) => A): A =
    parse(json => json.jdecode[X].result.fold(
      { case (msg, history) =>  decodefailure(msg, history) },
      a => success(a)
    ), errors => parsefailure(errors))

  /**
   * Parses this string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param success Run this function if the parse produces a success.
   * @param default Return this value of the parse or decode fails.
   */
  def parseDecodeOr[A, X: DecodeJson](success: X => A, default: => A): A =
    parseDecodeWith[A, X](success, _ => default, (_, _) => default)

  /**
   * Parses this string value into a JSON value and if it succeeds, decodes to a data-type.
   *
   * @param parsefailure Use this function to lift parser failures into decode failures.
   */
  def parseDecodeLiftParseFailures[X: DecodeJson](parsefailure: NonEmptyList[String] => String): DecodeResult[X] =
    parseDecodeWith[DecodeResult[X], X](
      x => DecodeResult(x),
      errors => DecodeResult.failedResult(parsefailure(errors), CursorHistory.build(Nil)),
      (msg, history) => DecodeResult.failedResult(msg, history))

  /**
   * Parses this string value and executes one of the given functions, depending on the parse outcome. Any error message is ignored.
   *
   * @param success Run this function if the parse succeeds.
   * @param failure Run this function if the parse produces a failure.
   */
  def parseIgnoreError[X](success: Json => X, failure: => X): X = parse(success, _ => failure)

  /**
   * Map the given function across the parsed result for a JSON value.
   */
  def parseTo[X](i: Json => X): ValidationNEL[String, X] = parse.map(i)

  /**
   * Runs the given function across a parser for a JSON value after successfully parsing. If the parser fails, then this
   * function will fail. ''WARNING: Partial Function''
   */
  def parseToOrDie[X](i: Json => X): X = {
    parseTo(i).fold[X](
      errors => sys.error("Unsuccessful parse result: " + errors.shows),
      identity
    )
  }

  /**
   * Parses this string value to a possible JSON value.
   */
  def pparse: Option[Json] = parseIgnoreError(Some(_), None)

  /**
   * Parses and decodes this string value to a possible JSON value.
   */
  def pparseDecode[X: DecodeJson]: Option[X] = pparse.flatMap(_.jdecode[X].value)

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
