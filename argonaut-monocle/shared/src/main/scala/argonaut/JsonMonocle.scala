package argonaut

import argonaut.Json.*
import argonaut.JsonObjectMonocle.*
import monocle.function.Each
import monocle.function.Plated
import monocle.Prism
import monocle.Traversal
import cats.Applicative
import cats.syntax.all.*

object JsonMonocle extends JsonMonocles

trait JsonMonocles {

  /** A Prism for JSON Null values. */
  val jNullPrism: Prism[Json, Unit] =
    Prism[Json, Unit](
      _.fold(Some(()), _ => None, _ => None, _ => None, _ => None, _ => None)
    )(_ => jNull)

  /** A Prism for JSON boolean values. */
  val jBoolPrism: Prism[Json, JsonBoolean] =
    Prism[Json, JsonBoolean](
      _.fold(None, b => Some(b), _ => None, _ => None, _ => None, _ => None)
    )(jBool)

  /** A Prism for JSON number values. */
  val jNumberPrism: Prism[Json, JsonNumber] =
    Prism[Json, JsonNumber](
      _.fold(None, _ => None, n => Some(n), _ => None, _ => None, _ => None)
    )(jNumber)

  /** A Prism for JSON number values. */
  val jBigDecimalPrism: Prism[Json, BigDecimal] =
    jNumberPrism andThen JsonNumberMonocle.jNumberToBigDecimal

  /**
   * An Optional for JSON number values based on Doubles.
   */
  // val jDoubleOptional: Optional[Json, Double] =
  //   jNumberPrism andThen JsonNumberMonocle.jNumberToDouble

  /**
   * An Optional for JSON number values based on Floats.
   */
  // val jFloatOptional: Optional[Json, Float] =
  //   jNumberPrism andThen JsonNumberMonocle.jNumberToFloat

  /** A Prism for JSON BigInt values. */
  val jBigIntPrism: Prism[Json, BigInt] =
    jNumberPrism andThen JsonNumberMonocle.jNumberToBigInt

  /** A Prism for JSON Long values. */
  val jLongPrism: Prism[Json, Long] =
    jNumberPrism andThen JsonNumberMonocle.jNumberToLong

  /**  A Prism for JSON Int values. */
  val jIntPrism: Prism[Json, Int] =
    jNumberPrism andThen JsonNumberMonocle.jNumberToInt

  /** A Prism for JSON Short values. */
  val jShortPrism: Prism[Json, Short] =
    jNumberPrism andThen JsonNumberMonocle.jNumberToShort

  /** A Prism for JSON Byte values. */
  def jBytePrism: Prism[Json, Byte] =
    jNumberPrism andThen JsonNumberMonocle.jNumberToByte

  /** A Prism for JSON string values. */
  val jStringPrism: Prism[Json, JsonString] =
    Prism[Json, JsonString](
      _.fold(None, _ => None, _ => None, s => Some(s), _ => None, _ => None)
    )(jString)

  /** A Prism for JSON array values. */
  val jArrayPrism: Prism[Json, JsonArray] =
    Prism[Json, JsonArray](
      _.fold(None, _ => None, _ => None, _ => None, a => Some(a), _ => None)
    )(jArray)

  /** A Prism for JSON object values. */
  val jObjectPrism: Prism[Json, JsonObject] =
    Prism[Json, JsonObject](
      _.fold(None, _ => None, _ => None, _ => None, _ => None, o => Some(o))
    )(jObject)

  /** a Traversal to all values of a JsonObject or JsonList */
  val jDescendants: Traversal[Json, Json] = new Traversal[Json, Json] {
    override def modifyA[F[_]](f: Json => F[Json])(s: Json)(implicit F: Applicative[F]): F[Json] =
      s.fold(
        F.pure(s),
        _ => F.pure(s),
        _ => F.pure(s),
        _ => F.pure(s),
        arr => Each.each[List[Json], Json].modifyA(f)(arr).map(Json.array(_*)),
        obj => Each.each[JsonObject, Json].modifyA(f)(obj).map(Json.jObject)
      )
  }

  implicit lazy val jsonPlated: Plated[Json] = new Plated[Json] {
    val plate: Traversal[Json, Json] = new Traversal[Json, Json] {
      def modifyA[F[_]](f: Json => F[Json])(a: Json)(implicit F: Applicative[F]): F[Json] = {
        a.fold(
          F.pure(a),
          b => F.pure(jBool(b)),
          n => F.pure(jNumber(n)),
          s => F.pure(jString(s)),
          _.traverse(f).map(jArray),
          JsonObjectCats.traverse(_, f).map(jObject)
        )
      }
    }
  }

}
