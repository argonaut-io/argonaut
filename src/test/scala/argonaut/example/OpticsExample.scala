package argonaut.example

import argonaut.Json._
import argonaut.StringWrap._
import argonaut.{Json, JsonObject, Parse}
import monocle.Monocle._
import org.specs2.mutable.Specification

import scalaz.{IList, Maybe}


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
      Parse.parseOptional.getMaybe(jsonStr) must_== Maybe.just(json)
    }

    "safe cast json to Boolean" in {
      jBoolPrism.getMaybe(jBool(true)) must_== Maybe.just(true)
      jBoolPrism.getMaybe(jBool(false)) must_== Maybe.just(false)
      jBoolPrism.getMaybe(jNumberOrString(3.5)) must_== Maybe.empty

      jBoolPrism.modify(!_)(jBool(true)) mustEqual jBool(false)
    }

    "safe cast json to String" in {
      jStringPrism.getMaybe(jString("test")) must_== Maybe.just("test")
      jStringPrism.getMaybe(jBool(true)) must_== Maybe.empty
    }

    "safe cast json to Double" in {
      jDoublePrism.getMaybe(jNumberOrString(3.5)) must_== Maybe.just(3.5)
      jDoublePrism.getMaybe(jBool(true)) must_== Maybe.empty
    }

    "safe cast json to Int" ! {
      jIntPrism.getMaybe(jNumberOrString(2)) must_== Maybe.just(2)
      jIntPrism.getMaybe(jNumberOrString(3.5)) must_== Maybe.empty
      jIntPrism.getMaybe(jBool(true)) must_== Maybe.empty
    }

    "safe cast json to JArray" in {
      val array = List(jBool(true), jString("test"))
      jArrayPrism.getMaybe(jArray(array)) must_== Maybe.just(array)
      jArrayPrism.getMaybe(jBool(true))   must_== Maybe.empty
    }

    "safe cast json to JObject" in {
      val obj = JsonObject.empty.+:("name" -> jString("Jon")).+:("age" -> jNumberOrString(23))
      jObjectPrism.getMaybe(jObject(obj)) must_== Maybe.just(obj)
      jObjectPrism.getMaybe(jBool(true)) must_== Maybe.empty
    }

    "at / index" in {
      (jObjectPrism composeLens at("first_name")).getMaybe(json)        must_== Maybe.just(Maybe.just(jString("fred")))
      (jObjectPrism composeOptional index("first_name")).getMaybe(json) must_== Maybe.just(jString("fred"))

      (jObjectPrism composeLens at("first_name")).set(Maybe.empty)(json) mustEqual Json(
        "last_name" := "munch",
        "age" := 23
      )
    }

    "filter index" in {
      (jObjectPrism composeTraversal
        filterIndex{f: JsonField => f.contains("name")} composePrism
        jStringPrism
      ).set("unknown")(json) must_== Json(
        "first_name" := "unknown",
        "last_name"  := "unknown",
        "age" := 23
        )
    }

    "each" in {
      (jObjectPrism composeTraversal each composePrism jStringPrism).getAll(json) must_== IList("fred", "munch")

      (jArrayPrism composeTraversal each composePrism jIntPrism)
        .getAll(jArray(List(jNumberOrString(1), jNumberOrString(2)))) must_== IList(1, 2)

      (jObjectPrism composeTraversal each composePrism jStringPrism composeOptional headMaybe)
        .modify(_.toUpper)(json) must_== Json(
         "first_name" := "Fred",
         "last_name" := "Munch",
         "age" := 23
       )
     }
  }
}
