package com.ephox
package argonaut

trait JsonIdentity[J] {
  val j: J

  def encode(implicit e: EncodeJson[J]): Json =
    e(j)
}

object JsonIdentity extends JsonIdentitys

trait JsonIdentitys {
  implicit def ToJsonIdentity[J](k: J): JsonIdentity[J] =
    new JsonIdentity[J] {
      val j = k  
    }
  
  implicit def FromJsonIdentity[J](k: JsonIdentity[J]): J =
    k.j
}