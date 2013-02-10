package argonaut.doc

import argonaut._, Argonaut._

object LensExample extends App {
  val jsonString: Json = jString("JSON!")
  // Get the possible value from the lens, this would return None if the instance wasn't a string.
  println(jStringPL.get(jsonString))

  // Create a two level nested object.
  val innerObject: Json = ("innerkey1", jString("innervalue1")) ->:
    ("innerkey2", jString("innervalue2")) ->:
    jEmptyObject
  val complexObject: Json = ("outerkey1", innerObject) ->:
    ("outerkey2", jFalse) ->:
    jEmptyObject

  val innerKey2StringLens = jObjectPL >=>   // Lens composition starts with converting to object...
    jsonObjectPL("outerkey1") >=>           // ...Looking up the "outerkey1" field...
    jObjectPL >=>                           // ...Converting to an object...
    jsonObjectPL("innerkey2") >=>           // ...Looking up the "innerkey2" field...
    jStringPL                               // ...Converting to a string.

  // Gets the value from deep within the structure.
  println(innerKey2StringLens.get(complexObject))

  // Modifies the inner most value returning entirely new Json instance.
  println(innerKey2StringLens.mod(_ + " is innervalue2", complexObject))
}
