package argonaut.doc

import argonaut._, Argonaut._

object ParseWithFailure extends App {

  val invalidjson = """
   { broken : hello,
  """

  val errors = Parse.parseWith[List[String]](invalidjson, _ => Nil, _.list)

  println("Parse errors: ")
  errors.foreach(println)

}
