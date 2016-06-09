package argonaut

import argonaut.Argonaut._
import org.specs2.mutable.Specification

object JsonPathSpecification extends Specification {

  val john: Json = Json.obj(
    "first_name" -> "John".asJson,
    "last_name"  -> "Doe".asJson,
    "age"        -> 25.asJson,
    "address"    -> Json.obj(
      "street_number" -> 12.asJson,
      "street_name"   -> "High Street".asJson
    ),
    "cars" -> Json.array(
      Json.obj(
        "model"     -> "fancy".asJson,
        "maxSpeed"  -> 120.asJson,
        "automatic" -> false.asJson
      ),
      Json.obj(
        "model"     -> "suv".asJson,
        "maxSpeed"  -> 80.asJson,
        "automatic" -> true.asJson
      )
    ).asJson
  )

  "JsonPath" >> {
    "support traversal by field name" >> {
      JsonPath.root.address.street_number.int.getOption(john) must_== Some(12)
    }

    "support traversal by array index" >> {
      JsonPath.root.cars.index(1).model.string.getOption(john) must_== Some("suv")
    }

    "support insertion and deletion" >> {
      JsonPath.root.at("first_name").setOption(None)(john) must_== john.obj.map(_.-("first_name")).map(jObject)
      JsonPath.root.at("foo").set(Some(true.asJson))(john).obj.flatMap(_.apply("foo")) must_== Some(jTrue)
    }
  }

  "JsonTraversalPath" >> {
    "support traversal over each values of a json object" >> {
      JsonPath.root.each.string.getAll(john) must_== List("John", "Doe")
    }

    "support traversal over each values of a json array" >> {
      JsonPath.root.cars.each.maxSpeed.int.getAll(john) must_== List(120, 80)
    }

    "support filtering of json object" >> {
      JsonPath.root.objFilter(_.contains("first")).string.getAll(john) must_== List("John")
    }

    "support filtering of json array" >> {
      JsonPath.root.cars.arrFilter(_ % 2 == 1).model.string.getAll(john) must_== List("suv")
    }
  }
}
