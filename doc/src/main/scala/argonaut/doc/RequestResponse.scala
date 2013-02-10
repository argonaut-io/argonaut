package argonaut.doc

import scalaz._, Scalaz._
import argonaut._, Argonaut._

object RequestResponse extends App {
  val requestJson =
    """
      |{
      |   "userid": "1"
      |}
      |""".stripMargin

  val updatedJson: Option[Json] = for {
    parsed <- requestJson.parseOption                     // Parse the JSON.
    modified = ("name", jString("testuser")) ->: parsed   // Prepend a name field into the JSON.
  } yield modified

  // If there was a failure at any point, provide a default.
  val responseJson: Json = updatedJson.getOrElse{
    jSingleObject("error", jString("Something went wrong."))
  }

  println(responseJson.nospaces)
}
