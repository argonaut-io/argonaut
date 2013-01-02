import argonaut._, Argonaut._

val invalidjson = """
 { broken : hello,
"""

val errors = Parse.parseWith[List[String]](json, _ => Nil, _.list)

println("Parse errors: ")
errors.foreach(println)
