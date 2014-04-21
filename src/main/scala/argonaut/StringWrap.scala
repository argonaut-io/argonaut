package argonaut

import scalaz._, Scalaz._

/**
 * Wraps a `String` value and provides methods, particularly for parsing.
 *
 * @author Tony Morris
 */
sealed trait StringWrap {
  /**
   * Underlying string value.
   */
  val value: String

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
   * This is an alias for `:?=` which is now preferred due to
   * better precendence.
   *
   * Example:
   * {{{
   *   ("key1" :=? None) ->?: ("key2" :=? Some("value2")) ->?: jEmptyObject
   * }}}
   */
  def :=?[A: EncodeJson](a: Option[A]) =
    this :?=(a)


  /*
   * Construct an optional pair of the key and JSON value.
   *
   * Example:
   * {{{
   *   ("key1" :?= None) ->?: ("key2" :?= Some("value2")) ->?: jEmptyObject
   * }}}
   */
  def :?=[A: EncodeJson](a: Option[A]) =
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
  implicit def StringToStringWrap(s: String): StringWrap =
    new StringWrap {
      val value = s
    }

  /**
   * Implicitly wraps the given string value with parse API.
   */
  implicit def StringToParseWrap(s: String): ParseWrap[String] =
    new ParseWrap(s, Parse)
}
