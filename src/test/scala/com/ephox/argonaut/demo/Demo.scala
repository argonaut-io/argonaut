package com.ephox.argonaut.demo

import com.ephox.argonaut.StringWrap._
import com.ephox.argonaut.Identity._
import com.ephox.argonaut.{ContraPossibleJson, Json, PossibleJson}

object Demo {

  def d[A : ContraPossibleJson](k: A) {
    import Json._

    val j = k.pjson

    // searches down through objects with the keys and obtains the number value at that level
    val a: Option[JsonNumber] = j -| "abc" -| "def" number

    // searches down through objects with the list of keys and returns the JSON string value or the empty string
    val b: JsonString = j -|| List("ghi", "jkl", "mno") stringOrEmpty

    // If it is a JSON object a "abc" field
    val c: Boolean = j hasField "pqr"

    // If it is a number, add ten to it
    val d: PossibleJson = j withNumber (10+)

    // If it is a JSON array, return it, or default to List(jNull, jTrue)
    val e: JsonArray = j arrayOr (List(jNull, jTrue))

    // If it is a JSON object, with a field "xyz" that is a JSON number, return it, otherwise, default to 42
    val f: JsonNumber = j -| "xyz" numberOr 42D

    // If it is a JSON object, prepend the given key/value pairs to it
    val g: PossibleJson = ("k1", jString("v1")) ->: ("k2", jTrue) ->: j

    // If it is a JSON array, prepend the given JSON values to it
    val h: PossibleJson = jFalse -->>: jString("boo") -->>: j

    // If it is a JSON number, return a List containing "cuisine" that many times, otherwise List("brochure", "bakery")
    val i: List[String] = j usingNumber (n => List.fill(n.toInt)("cuisine"), List("brochure", "bakery"))

    List(("a", a), ("b", b), ("c", c), ("d", d), ("e", e), ("f", f), ("g", g), ("h", h), ("i", i)) foreach { case (x, y) => println(x + " : " + y) }
    println
  }

  def demo(j: List[String]) {
    j map (_.parse) foreach (d(_))
  }

  def main(args: Array[String]) {
    val jsons = List(
      "true"
    , "[ true ]"
    , "8"
    , """[ "chook1", "chook2" ]"""
    , """{ "chook1" : "chook2" }"""
    , """
      {
        "abc" :
          {
            "def" : 7
          },
        "ghi" :
          {
            "ata" : null,
            "jkl" :
              {
                "mno" : "argo"
              }
          },
        "pqr" : false,
        "operator": "is",
        "values": ["cat", "dog", "rabbit"],
        "xyz" : 24
      }
      """
    )

    demo(jsons)
  }
}
