package argonaut.example

import argonaut._, Argonaut._

object JsonExample extends ArgonautSpec {
  val json =
    Json(
      "name" := "fred",
      "age" := 23,
      "wallet" := List(
        Json { "value" := 100 },
        Json { "value" := 10 },
        Json { "value" := 50 }
      )
    )

  val value =
    Person("fred", 23, List(Coin(100), Coin(10), Coin(50)))

  case class Coin(value: Int)
  case class Person(name: String, age: Int, wallet: List[Coin])

  implicit val CodecCoin: CodecJson[Coin] = casecodec1(Coin.apply, (a: Coin) => Option(a.value))("value")
  implicit val CodecPerson: CodecJson[Person] = casecodec3(Person.apply, Person.unapply)("name", "age", "wallet")

  def is = s2"""
  Can decode hand crafted object ${
    json.as[Person].toOption must beSome(value)
  }
  Can encode to match hand crafted object ${
    value.asJson must_== json
  }
  """
}
