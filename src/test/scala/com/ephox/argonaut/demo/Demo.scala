package com.ephox.argonaut.demo

import com.ephox.argonaut.{Json, JsonParser}

object Demo {

  class Chicken(val name: String)

  def demo(j: Json) {
    import Json._

    // searches down through objects with the keys and obtains the number value at that level
    val a: Option[JsonNumber] = j -| "abc" -| "def" number

    // searches down through objects with the list of keys and returns the JSON string value or the empty string
    val b: JsonString = j -|| List("abc", "def", "ghi") stringOrEmpty

    // If it is a JSON object a "abc" field
    val c: Boolean = j hasField "abc"

    // If it is a number, add one to it
    val d: Json = j withNumber (1+)

    // If it is a JSON array, return it, or default to List(jNull, jTrue)
    val e: JsonArray = j arrayOr (List(jNull, jTrue))

    // If it is a JSON object, with a field "xyz" that is a JSON number, return it, otherwise, default to 42
    val f: JsonNumber = j -| "xyz" numberOr 42D

    // If it is a JSON object, prepend the given key/value pairs to it
    val g: Json = ("k1", jString("v1")) ->: ("k2", jTrue) ->: j

    // If it is a JSON array, prepend the given JSON values to it
    val h: Json = jFalse -->>: jString("boo") -->>: j

    // If it is a JSON number, return a List containing "cuisine" that many times, otherwise List("brochure", "bakery")
    val i: List[String] = j usingNumber (n => List.fill(n.toInt)("cuisine"), List("brochure", "bakery"))

    List(a, b, c, d, e, f, g, h, i) foreach println
  }







  def main(args: Array[String]) {


//    val result = JsonParser.parse("[\"chook1\", \"chook2\"]")
//    val json = result.get

    // val chooks = json.array.map(_.map())
    // println(chooks)

    /*
    => Json
    json(array) => list
       json(string) => Chicken



     */

    // Option[List[Chicken]] chooks = JsonParser.parseTo(json, list(string, new Chicken(_)))

    // List[String] => Option[List[Chicken]]

    // parse: String => ParseResult[Json]

    // parse2: String => Option[Json]

    // f: JsonKleisli[Chicken]

    // p = s: String => parse(s) map f : ParseResult[Option[Chicken]]


    def parse: String => Option[Json] = JsonParser.parseOptional(_)

    val input = "[\"chook1\", \"chook2\"]";
    val result: Option[Json] = JsonParser.parseOptional(input)

    val stringToChicken = new Chicken(_)
    //val o: Option[List[Chicken]] = result flatMap (_.array flatMap (JsonKleisli.mapMOption(_, (_: Json).string map stringToChicken)))

    val stringToMaybeChicken = (s: String) => Some(new Chicken(s))

//    val o: Option[List[Chicken]] = result flatMap (_.array flatMap (JsonKleisli.mapMOption(_, (_: Json).string map stringToChicken)))
  //  val p: Option[List[Chicken]] = result flatMap (_.array flatMap (JsonKleisli.mapMOption(_, (_: Json).string flatMap stringToMaybeChicken)))


//    val z = arrayOf(string map stringToChicken) =<< result

 //   val z1 = arrayOf(string) =<< result



    class Field(val name: String)
    class Operator(val name: String)
    class Value(val value: String)

    class FOV(val field: Field, val op: Operator, val values: List[Value])


    val in = """
    {
      "field": "title",
      "operator": "is",
      "values": ["cat", "dog", "rabbit"]
    }
    """

    val res: Option[Json] = JsonParser.parseOptional(in)

/*
    class OptionJsonField(oj: Option[Json], field: String) {
      def as[T](f: Json => Option[T]): Option[T] = oj flatMap(_ objectValue field) flatMap f
      def asString = as(_.string)
      def asListOfString = error("argh")
    }

    class RichOptionJson(oj: Option[Json]) {
      def obj(field: String): OptionJsonField = new OptionJsonField(oj, field)
    }

    implicit def toRichOptionJson = new RichOptionJson(_)
*/

    //[("field", string), ("operator", string), ("value", list(string))]

   //  def q(j: Json, s: String) = (res flatMap (_ objectValue "field"))

//    val f: Option[String] = res flatMap (_ objectValueString "field")
//    val o: Option[String] = res flatMap (_ objectValueString "is")
//    val v: Option[String] = (res flatMap (_.objectMap) flatMap (_ get "field")) flatMap (_.string)


    //res.array.string()
 //    val f: Option[String] = res.obj("field").asString
  //   val o: Option[String] = res.obj("is").asString
 //   val v: Option[List[String]] = res.obj("is").asListOfString

//    val v: Option[List[String]] = res.obj()


//    val f: Option[String] = json field string "field"
//    val o: Option[String] = json field string "is"
//    val v: Option[List[String]] = json field list string "values"


    // res2("operator")

    /*





     */


//    val q: FOV = objectOf(List("field")) =<< res




  }

}
