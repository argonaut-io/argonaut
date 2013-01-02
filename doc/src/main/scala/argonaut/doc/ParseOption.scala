package argonaut.doc

import argonaut._, Argonaut._

object ParseOption extends App {

  val json = """
    { "a" : 1, "b" : 2 }
  """

  val result = Parse.parseOption(json)

  result.foreach(println)

}
