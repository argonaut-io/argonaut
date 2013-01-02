import argonaut._, Argonaut._

val json = """
  [ "thing1", "thing2", "cat", "hat" ]
"""

val result = Parse.parseOr(json, _.arrayOrEmpty, Nil)

println("Valid list elements: ")
result.foreach(println)
