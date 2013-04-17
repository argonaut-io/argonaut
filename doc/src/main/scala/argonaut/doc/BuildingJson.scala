package argonaut.doc

import argonaut._, Argonaut._

object BuildingJson extends App {
  val jsonString: Json = jString("JSON!")
  val jsonNumber: Json = jNumber(20)
  val jsonNull: Json = jNull
  val jsonBoolean: Json = jBool(true)
  // jTrue and jFalse create the boolean true and false JSON representations directly.
  val jsonArray: Json = jArray(List(jsonNumber, jsonString))
  // jEmptyArray and jSingleArray provide more specific implementations.
  val jsonObject: Json = jObjectAssocList(List(("key1", jsonNumber), ("key2", jsonString)))

  // Creating arrays and objects looks much cleaner when using methods available on the Json type.
  // As instances of the Json type are immutable it's completely safe
  // to add fields or values to existing instances without affecting the original.
  val easyJsonArray: Json = jsonString -->>: jsonNumber -->>: jEmptyArray
  val easyJsonObject: Json = ("key1", jsonNumber) ->: ("key2", jsonString) ->: jEmptyObject
}
