package argonaut

import argonaut.Argonaut._
import org.specs2.mutable.Specification
import argonaut.JsonPath.root

object JsonPathSpecification extends Specification {

  case class Car(model: String, maxSpeed: Int, automatic: Boolean)
  object  Car {
    implicit val codec: CodecJson[Car] = CodecJson.casecodec3(Car.apply, Car.unapply)("model", "maxSpeed", "automatic")
  }

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
      root.address.street_number.int.getOption(john) must_== Some(12)
    }

    "support traversal by array index" >> {
      root.cars.index(1).model.string.getOption(john) must_== Some("suv")
    }

    "support traversal by array index using apply" >> {
      root.cars(1).model.string.getOption(john) must_== Some("suv")
    }

    "support traversal by array index using apply on the root" >> {
      val jsonArray = Json.array("first".asJson, "second".asJson).asJson
      root(0).string.getOption(jsonArray) must_== Some("first")
    }

    "support insertion and deletion" >> {
      root.at("first_name").setOption(None)(john) must_== john.obj.map(_.-("first_name")).map(jObject)
      root.at("foo").set(Some(true.asJson))(john).obj.flatMap(_.apply("foo")) must_== Some(jTrue)
    }

    "support parsing json using a codec" >> {
      root.cars.index(0).as[Car].getAll(john) must_== List(Car("fancy", 120, automatic = false))
    }
  }

  "JsonTraversalPath" >> {
    "support traversal over each values of a json object" >> {
      root.each.string.getAll(john) must_== List("John", "Doe")
    }

    "support traversal over each values of a json array" >> {
      root.cars.each.maxSpeed.int.getAll(john) must_== List(120, 80)
    }

    "support traversal over each values of a json array" >> {
      root.cars.each.maxSpeed.int.getAll(john) must_== List(120, 80)
    }

    "support filtering by field of json object" >> {
      root.filterByField(_.contains("first")).string.getAll(john) must_== List("John")
    }

    "support filtering by index of json array" >> {
      root.cars.filterByIndex(_ % 2 == 1).as[Car].getAll(john) must_== List(Car("suv", 80, automatic = true))
    }

    "support a safe filtering by value" >> {
      root.cars.each.filter(root.maxSpeed.int.asFold.exist(_ > 100)(_)).model.string.getAll(john) must_== List("fancy")
    }

  }
}
