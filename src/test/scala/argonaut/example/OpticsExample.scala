package argonaut.example

import argonaut.Json._
import argonaut.StringWrap._
import argonaut.{Json, JsonObject, Parse}
import monocle.Monocle._
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
      Parse.parseOptional.getOption(jsonStr) must_== Some(json)
    }

    "safe cast json to Boolean" in {
      jBoolPrism.getOption(jBool(true)) must_== Some(true)
      jBoolPrism.getOption(jBool(false)) must_== Some(false)
      jBoolPrism.getOption(jNumberOrString(3.5)) must_== None

      jBoolPrism.modify(!_)(jBool(true)) mustEqual jBool(false)
    }

    "safe cast json to String" in {
      jStringPrism.getOption(jString("test")) must_== Some("test")
      jStringPrism.getOption(jBool(true)) must_== None
    }

    "safe cast json to Double" in {
      jDoublePrism.getOption(jNumberOrString(3.5)) must_== Some(3.5)
      jDoublePrism.getOption(jBool(true)) must_== None
    }

    "safe cast json to Int" ! {
      jIntPrism.getOption(jNumberOrString(2)) must_== Some(2)
      jIntPrism.getOption(jNumberOrString(3.5)) must_== None
      jIntPrism.getOption(jBool(true)) must_== None
    }

    "safe cast json to JArray" in {
      val array = List(jBool(true), jString("test"))
      jArrayPrism.getOption(jArray(array)) must_== Some(array)
      jArrayPrism.getOption(jBool(true))   must_== None
    }

    "safe cast json to JObject" in {
      val obj = JsonObject.empty.+:("name" -> jString("Jon")).+:("age" -> jNumberOrString(23))
      jObjectPrism.getOption(jObject(obj)) must_== Some(obj)
      jObjectPrism.getOption(jBool(true)) must_== None
    }

    "at / index" in {
      (jObjectPrism composeLens at("first_name")).getOption(json)        must_== Some(Some(jString("fred")))
      (jObjectPrism composeOptional index("first_name")).getOption(json) must_== Some(jString("fred"))

      (jObjectPrism composeLens at("first_name")).set(None)(json) mustEqual Json(
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
      (jObjectPrism composeTraversal each composePrism jStringPrism).getAll(json) must_==
        List("fred", "munch")

      (jArrayPrism composeTraversal each composePrism jIntPrism)
        .getAll(jArray(List(jNumberOrString(1), jNumberOrString(2)))) must_== List(1, 2)

      (jObjectPrism composeTraversal each composePrism jStringPrism composeOptional headOption)
        .modify(_.toUpper)(json) must_== Json(
         "first_name" := "Fred",
         "last_name" := "Munch",
         "age" := 23
       )
     }
  }
}
