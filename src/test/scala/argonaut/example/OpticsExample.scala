package argonaut.example

import argonaut.Json._
import argonaut.StringWrap._
import argonaut.{JsonObject, Parse, Json}
import monocle.function.At._
import monocle.function.Each._
import monocle.syntax._
import org.specs2._
import org.specs2.specification._


object OpticsExample extends Specification {

  val jsonStr = """
                  |{
                  |  "first_name" : "fred",
                  |  "last_name"  : "munch",
                  |  "age"        : 23
                  |}
                """.stripMargin

  val json = Json(
    "first_name" := "fred",
    "last_name"  := "munch",
    "age"        := 23
  )

  def is = "Optics" ^
    "parse string to Json" ! {
      Parse.parseOptional.getOption(jsonStr) must beSome(json)
    } ^
    "safe cast json to Boolean" ! {
      jBoolPrism.getOption(jBool(true))          must_==(Some(true))
      jBoolPrism.getOption(jBool(false))         must_==(Some(false))
      jBoolPrism.getOption(jNumberOrString(3.5)) must_==(None)

      jBoolPrism.modify(jBool(true), !_) must_==(jBool(false))
    } ^
    "safe cast json to String" ! {
      jStringPrism.getOption(jString("test")) must_==(Some("test"))
      jStringPrism.getOption(jBool(true))     must_==(None)
    } ^
    "safe cast json to Double" ! {
      jDoublePrism.getOption(jNumberOrString(3.5)) must_==(Some(3.5))
      jDoublePrism.getOption(jBool(true))          must_==(None)
    } ^
    "safe cast json to Int" ! {
      jIntPrism.getOption(jNumberOrString(2))   must_==(Some(2))
      jIntPrism.getOption(jNumberOrString(3.5)) must_==(None)
      jIntPrism.getOption(jBool(true))          must_==(None)
    } ^
    "safe cast json to JArray" ! {
      val array = List(jBool(true), jString("test"))
      jArrayPrism.getOption(jArray(array)) must_==(Some(array))
      jArrayPrism.getOption(jBool(true))   must_==(None)
    } ^
    "safe cast json to JObject" ! {
      val obj = JsonObject.empty.+:("name" -> jString("Jon")).+:("age" -> jNumberOrString(23))
      jObjectPrism.getOption(jObject(obj)) must_==(Some(obj))
      jObjectPrism.getOption(jBool(true))  must_==(None)
    } ^
    "Example of at" ! {
      (json <-? jObjectPrism |-? at("first_name") getOption) must_==(Some("fred"))

      (json <-? jObjectPrism |-? at("first_name") set None) must_== (Json(
        "last_name"  := "munch",
        "age"        := 23
      ))
    } ^
    "Example of each" ! {
      (json <-? jObjectPrism |->> each |->> jStringPrism getAll) must_==(List("fred", "munch"))

      (jArray(List(jNumberOrString(1),jNumberOrString(2))) <-? jArrayPrism |->> each |->> jIntPrism getAll) must_==(List(1,2,3))

      (json <-? jObjectPrism |->> each |->> jStringPrism modify (_.capitalize)) must_==(Json(
        "first_name" := "Fred",
        "last_name"  := "Munch",
        "age"        := 23
      ))
    }
}
