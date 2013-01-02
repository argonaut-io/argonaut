package argonaut.doc

import argonaut._, Argonaut._

object ParseValidation extends App {
  val result = Parse.parse("""
    {
      "name" : "bob",
      "age" : 49
    }
  """)

  println(if (result.isSuccess) "Parse was successfull" else "Parse failed")
}
