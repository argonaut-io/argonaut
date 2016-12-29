package argonaut

trait JsonIdentity[J] {
  val j: J

  /**
   * Encode to a JSON value using the given implicit encoder.
   */
  def jencode(implicit e: EncodeJson[J]): Json = e(j)

  /**
   * Encode to a JSON value using the given implicit encoder. Alias for `jencode`.
   */
  def asJson(implicit e: EncodeJson[J]): Json = jencode

  /**
    * Encode to a JSONNumber.
    */
  def asJsonNumber(implicit asn: EncodeJsonNumber[J]): JsonNumber = asn.encodeJsonNumber(j)

  /**
    * Encode to a JSONNumber, wrapped in a Some if it is valid, otherwise a None.
    */
  def asPossibleJsonNumber(implicit asn: EncodePossibleJsonNumber[J]): Option[JsonNumber] = asn.possiblyEncodeJsonNumber(j)
}

object JsonIdentity extends JsonIdentitys

trait JsonIdentitys {
  implicit def ToJsonIdentity[J](k: J): JsonIdentity[J] = {
    new JsonIdentity[J] {
      val j = k
    }
  }

  implicit def FromJsonIdentity[J](k: JsonIdentity[J]): J = k.j
}
