package argonaut.doc

import scalaz._, Scalaz._
import argonaut._, Argonaut._

object RequestResponse extends App {

  case class User(userID: String, username: String)

  // Fake implementation.
  def lookupUser(userID: String): Option[User] = User(userID, "Sean").some

  val requestJson = """{ "userid" : "1" }"""

  val username: Option[String] = for {
    parsed <- requestJson.parseOption   // Parse the JSON.
    jsonObject <- parsed.obj            // Get the JSON as a JsonObject instance.
    userIDJson <- jsonObject("userid")  // Get the "userid" field from the JsonObject.
    userID <- userIDJson.string         // Get the value of the "userid" field.
    user <- lookupUser(userID)          // Get an instance of User for the user ID.
  } yield user.username                 // Retrieve the username we're interested in.

  // If there was a failure at any point, provide a default.
  val responseJson: Json = username.fold(
    name => jSingleObject("username", jString(name)),
    jSingleObject("error", jString("Something went wrong."))
  )

  println(responseJson.nospaces)

}
