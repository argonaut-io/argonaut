package argonaut.example

import argonaut.Json._
import argonaut.StringWrap._
import argonaut.{JsonObject, Parse, Json}
import monocle.function.At._
import monocle.function.Each._
import monocle.function.HeadOption._
import monocle.function.Index._
import monocle.function.FilterIndex._
import monocle.syntax._
import org.specs2.mutable.Specification


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

  "Optics"  should {

    "parse string to Json" in {
      Parse.parseOptional.getOption(jsonStr) must beSome(json)
    }

    "safe cast json to Boolean" in {
      jBoolPrism.getOption(jBool(true)) must beSome(true)
      jBoolPrism.getOption(jBool(false)) must beSome(false)
      jBoolPrism.getOption(jNumberOrString(3.5)) must beNone

      jBoolPrism.modify(jBool(true), !_) mustEqual jBool(false)
    }

    "safe cast json to String" in {
      jStringPrism.getOption(jString("test")) must beSome("test")
      jStringPrism.getOption(jBool(true)) must beNone
    }

    "safe cast json to Double" in {
      jDoublePrism.getOption(jNumberOrString(3.5)) must beSome(3.5)
      jDoublePrism.getOption(jBool(true)) must beNone
    }

    "safe cast json to Int" ! {
      jIntPrism.getOption(jNumberOrString(2)) must beSome(2)
      jIntPrism.getOption(jNumberOrString(3.5)) must beNone
      jIntPrism.getOption(jBool(true)) must beNone
    }

    "safe cast json to JArray" in {
      val array = List(jBool(true), jString("test"))
      jArrayPrism.getOption(jArray(array)) must_==(Some(array))
      jArrayPrism.getOption(jBool(true)) must beNone
    }

    "safe cast json to JObject" in {
      val obj = JsonObject.empty.+:("name" -> jString("Jon")).+:("age" -> jNumberOrString(23))
      jObjectPrism.getOption(jObject(obj)) must beSome(obj)
      jObjectPrism.getOption(jBool(true)) must beNone
    }

    "at / index" in {
      (json <-? jObjectPrism |-? at("first_name")    getOption) mustEqual Some(Some(jString("fred")))
      (json <-? jObjectPrism |-? index("first_name") getOption) must beSome(jString("fred"))

      (json <-? jObjectPrism |-? at("first_name") set None) mustEqual Json(
        "last_name" := "munch",
        "age" := 23
      )
    }

    "filter index" in {
      (json <-? jObjectPrism |->> filterIndex{f: JsonField => f.contains("name")} set jString("unknown")) mustEqual Json(
        "first_name" := "unknown",
        "last_name"  := "unknown",
        "age" := 23
        )
    }

    "each" in {
      (json <-? jObjectPrism |->> each |->> jStringPrism getAll) mustEqual List("fred", "munch")
      (jArray(List(jNumberOrString(1), jNumberOrString(2))) <-? jArrayPrism |->> each |->> jIntPrism getAll) mustEqual List(1, 2)

      (json <-? jObjectPrism |->> each |->> jStringPrism |->> headOption modify (_.toUpper)) mustEqual Json(
        "first_name" := "Fred",
        "last_name" := "Munch",
        "age" := 23
      )
    }
  }
}
