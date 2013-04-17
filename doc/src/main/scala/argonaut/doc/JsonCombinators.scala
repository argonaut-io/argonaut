package argonaut.doc

import argonaut._, Argonaut._

object JsonCombinators extends App {
  // withString modifies a string value otherwise leaves the Json instance unchanged.
  val appendedString: Json = jString("JSO").withString(_ + "N")
  // withNumber modifies a number value otherwise leaves the Json instance unchanged.
  val incrementedNumber: Json = jNumber(100).withNumber(_ + 10)
  // withObject modifies an object value otherwise leaves the Json instance unchanged.
  val modifiedObject: Json = jSingleObject("field", jTrue).withObject(_ - "field")
  // withArray modifies an array value otherwise leaves the Json instance unchanged.
  val modifiedArray: Json = jSingleArray(jTrue).withArray(_ :+ jFalse)
}
