package argonaut

import scalaz.*
import Scalaz.*
import Json.*
import JsonNumberScalaz.*
import JsonObjectScalaz.*

object JsonScalaz extends JsonScalazs {}

trait JsonScalazs {

  /**
   * A partial lens for JSON boolean values.
   */
  def jBoolPL: Json @?> Boolean =
    PLens(_.fold(None, z => Some(Store(JBool, z)), _ => None, _ => None, _ => None, _ => None))

  /**
   * A partial lens for JSON number values.
   */
  def jNumberPL: Json @?> JsonNumber =
    PLens(_.fold(None, _ => None, z => Some(Store(JNumber, z)), _ => None, _ => None, _ => None))

  /**
   * A partial lens for JSON string values.
   */
  def jStringPL: Json @?> JsonString =
    PLens(_.fold(None, _ => None, _ => None, z => Some(Store(JString, z)), _ => None, _ => None))

  /**
   * A partial lens for JSON array values.
   */
  def jArrayPL: Json @?> JsonArray =
    PLens(_.fold(None, _ => None, _ => None, _ => None, z => Some(Store(JArray, z)), _ => None))

  /**
   * A partial lens for JSON object values.
   */
  def jObjectPL: Json @?> JsonObject =
    PLens(_.fold(None, _ => None, _ => None, _ => None, _ => None, z => Some(Store(JObject, z))))

  /**
   * A partial lens for element of JSON array.
   */
  def jsonArrayPL(n: Int): JsonArray @?> Json =
    PLens(array => array lift n map (Store(array.updated(n, _), _)))

  implicit val JsonInstances: Equal[Json] & Show[Json] = {
    new Equal[Json] with Show[Json] {
      def equal(a1: Json, a2: Json) = {
        a1 match {
          case JNull => a2.isNull
          case JBool(b) => a2.bool exists (_ == b)
          case JNumber(n) => a2.number exists (_ === n)
          case JString(s) => a2.string exists (_ == s)
          case JArray(a) => a2.array exists (_ === a)
          case JObject(o) => a2.obj exists (_ === o)
        }
      }

      override def show(a: Json) = Show.showFromToString.show(a)
    }
  }

  /**
    * Decode `A` based on `HCursor => ValidationNel[String, A]` function.
    */
  def asWithValidation[A](f: HCursor => ValidationNel[String, A]): DecodeJson[A] =
    (c: HCursor) =>
      f(c) match {
        case Success(a) => DecodeResult.ok(a)
        case Failure(err) => DecodeResult.fail(err.shows, c.history)
      }

}
