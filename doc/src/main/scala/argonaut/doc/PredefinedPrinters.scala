package argonaut.doc

import argonaut._, Argonaut._

object PredefinedPrinters extends App {

  val json = ("red" := true) ->: ("blue" := false) ->: jEmptyObject

  println("""/** compact **/""")
  println(json.nospaces)

  println("""/** formatted with 2 spaces **/""")
  println(json.spaces2)

  println("""/** formatted with 4 spaces **/""")
  println(json.spaces4)

}
