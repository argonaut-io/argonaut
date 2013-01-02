package argonaut.doc

import argonaut._, Argonaut._

object ParseValidation extends App {

  val json = """
    {
      "name" : "bob",
      "age" : 49
    }
  """

  val result = Parse.parse(json)

  println(if (result.isSuccess) "Parse was successfull" else "Parse failed")

}
