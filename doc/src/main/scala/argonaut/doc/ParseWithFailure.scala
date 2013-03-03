package argonaut.doc

import argonaut._, Argonaut._

object ParseWithFailure extends App {

  val invalidjson = """
   { broken : hello,
  """

  val errors = Parse.parseWith[Option[String]](invalidjson, _ => None, Some.apply)

  println("Parse errors: ")
  errors.foreach(println)

}
