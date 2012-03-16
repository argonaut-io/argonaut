package com.ephox
package argonaut

import Json._
import PossibleJson._
import scalaz._, PLens._, CoStateT._

trait JsonLike[J] {
  /**
   * A partial lens for JSON boolean values.
   */
  def jBoolL: J @-? Boolean

  /**
   * A partial lens for JSON number values.
   */
  def jNumberL: J @-? JsonNumber

  /**
   * A partial lens for JSON string values.
   */
  def jStringL: J @-? JsonString

  /**
   * A partial lens for JSON string values.
   */
  def jArrayL: J @-? JsonArray

  /**
   * A partial lens for JSON string values.
   */
  def jObjectL: J @-? JsonObject

  /**
   * Construct a JSON value that is `null`.
   */
  def jNull: J
  
  /**
   * Construct a JSON value that is a boolean.
   */
  def jBool: Boolean => J

  /**
   * Construct a JSON value that is a number.
   */
  def jNumber: JsonNumber => J

  /**
   * Construct a JSON value that is a string.
   */
  def jString: String => J

  /**
   * Construct a JSON value that is an array.
   */
  def jArray: JsonArray => J

  /**
   * Construct a JSON value that is an object.
   */
  def jObject: JsonObject => J
}

object JsonLike extends JsonLikes

trait JsonLikes {

  implicit def PossibleJsonJsonLike: JsonLike[PossibleJson] =
    new JsonLike[PossibleJson] {
      def jBoolL: PossibleJson @-? Boolean =
        PLens(_.bool map (coState(jBool, _)))
    
      def jNumberL: PossibleJson @-? JsonNumber =
        PLens(_.number map (coState(jNumber, _)))
    
      def jStringL: PossibleJson @-? JsonString =
        PLens(_.string map (coState(jString, _)))
    
      def jArrayL: PossibleJson @-? JsonArray =
        PLens(_.array map (coState(jArray, _)))
    
      def jObjectL: PossibleJson @-? JsonObject =
        PLens(_.obj map (coState(jObject, _)))

      def jNull =
        pJson(jjNull)

      def jBool =
        pJson compose jjBool

      def jNumber =
        pJson compose jjNumber

      def jString =
        pJson compose jjString

      def jArray =
        pJson compose jjArray

      def jObject =
        pJson compose jjObject
    }

  implicit def JsonJsonLike: JsonLike[Json] =
    new JsonLike[Json] {
      def jBoolL: Json @-? Boolean =
        PLens(_.bool map (coState(jBool, _)))
    
      def jNumberL: Json @-? JsonNumber =
        PLens(_.number map (coState(jNumber, _)))
    
      def jStringL: Json @-? JsonString =
        PLens(_.string map (coState(jString, _)))
    
      def jArrayL: Json @-? JsonArray =
        PLens(_.array map (coState(jArray, _)))
    
      def jObjectL: Json @-? JsonObject =
        PLens(_.obj map (coState(jObject, _)))

      def jNull =
        jjNull

      def jBool =
        jjBool

      def jNumber =
        jjNumber

      def jString =
        jjString

      def jArray =
        jjArray

      def jObject =
        jjObject
    }
  
  /**
   * A partial lens for JSON boolean values.
   */
  def jBoolL[J](implicit l: JsonLike[J]): J @-? Boolean =
    l.jBoolL

  /**
   * A partial lens for JSON number values.
   */
  def jNumberL[J](implicit l: JsonLike[J]): J @-? JsonNumber =
    l.jNumberL

  /**
   * A partial lens for JSON string values.
   */
  def jStringL[J](implicit l: JsonLike[J]): J @-? JsonString =
    l.jStringL

  /**
   * A partial lens for JSON string values.
   */
  def jArrayL[J](implicit l: JsonLike[J]): J @-? JsonArray =
    l.jArrayL

  /**
   * A partial lens for JSON string values.
   */
  def jObjectL[J](implicit l: JsonLike[J]): J @-? JsonObject =
    l.jObjectL

  /**
   * Construct a JSON value that is `null`.
   */
  def jNull[J](implicit l: JsonLike[J]): J =
    l.jNull

  /**
   * Construct a JSON value that is a boolean.
   */
  def jBool[J](implicit l: JsonLike[J]): Boolean => J =
    l.jBool

  /**
   * Construct a JSON boolean value of `true`.
   */
  def jTrue[J](implicit l: JsonLike[J]): J =
    l.jBool(true)

  /**
   * Construct a JSON boolean value of `false`.
   */
  def jFalse[J](implicit l: JsonLike[J]): J =
    l.jBool(false)

  /**
   * Construct a JSON value that is a number.
   */
  def jNumber[J](implicit l: JsonLike[J]): JsonNumber => J =
    l.jNumber

  /**
   * A JSON value that is a zero number.
   */
  def jZero[J](implicit l: JsonLike[J]): J =
    l.jNumber(0D)

  /**
   * Construct a JSON value that is a string.
   */
  def jString[J](implicit l: JsonLike[J]): String => J =
    l.jString

  /**
   * A JSON value that is an empty string.
   */
  def jEmptyString[J](implicit l: JsonLike[J]): J =
    l.jString("")

  /**
   * Construct a JSON value that is an array.
   */
  def jArray[J](implicit l: JsonLike[J]): JsonArray => J =
    l.jArray

  /**
   * A JSON value that is an empty array.
   */
  def jEmptyArray[J](implicit l: JsonLike[J]): J =
    l.jArray(Nil)

  /**
   * Returns a function that takes a single value and produces a JSON array that contains only that value.
   */
  def jSingleArray[J](implicit l: JsonLike[J]): Json => J =
    j => l.jArray(List(j))

  /**
   * Construct a JSON value that is an object.
   */
  def jObject[J](implicit l: JsonLike[J]): JsonObject => J =
    l.jObject

  /**
   * A JSON value that is an empty object.
   */
  def jEmptyObject[J](implicit l: JsonLike[J]): J =
    l.jObject(Nil)

  /**
   * Returns a function that takes an association value and produces a JSON object that contains only that value.
   */
  def jSingleObject[J](implicit l: JsonLike[J]): JsonField => Json => J =
    k => v => l.jObject(List((k, v)))

  /**
   * Construct a JSON value that is an object from an index.
   */
  def jObjectMap[J](implicit l: JsonLike[J]): JsonObjectMap => J =
    x => l.jObject(x.toList)
  
}