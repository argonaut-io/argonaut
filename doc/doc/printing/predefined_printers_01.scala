import argonaut._, Argonaut._

val json = ("red" := true) ->: ("blue" := false) ->: jEmptyObject

println("""/** compact **/""")
println(json.nospaces)

println("""/** formatted with 2 spaces **/""")
println(json.spaces2)

println("""/** formatted with 4 spaces **/""")
println(json.spaces4)

