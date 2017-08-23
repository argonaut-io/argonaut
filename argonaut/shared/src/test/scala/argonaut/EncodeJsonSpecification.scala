package argonaut

import Argonaut._

object EncodeJsonSpecification extends ArgonautSpec { def is = s2"""
  EncodeJson mapJson
    Normal invocation                     ${mapJson}
  EncodeJson Witness Compilation
    Witness basics                        ${ok}
    Witness tuples                        ${ok}
    Witness auto                          ${ok}
    Witness derived                       ${ok}
  EncodeJson derive
    BackTicks                             ${derived.testBackTicksEncodeJson}
"""

  def mapJson = {
    prop{(key: String, n: Int) =>
      val json = (key, n.jencode) ->: jEmptyObject
      val encodeJson = EncodeJson.of[Int].mapJson(j => (key, j) ->: jEmptyObject)
      encodeJson.encode(n) must beEqualTo(json)
    }
  }

  object primitives {
    EncodeJson.of[String]
    EncodeJson.of[Int]
    EncodeJson.of[Boolean]
    EncodeJson.of[Long]
    EncodeJson.of[Double]
    EncodeJson.of[Short]
    EncodeJson.of[Byte]
    EncodeJson.of[Option[Int]]
    EncodeJson.of[Option[String]]
  }

  object tuples {
    EncodeJson.of[(String, Int)]
    EncodeJson.of[(String, Int, Boolean)]
    EncodeJson.of[(String, Int, Boolean, Long)]
    EncodeJson.of[(String, Int, Boolean, Long, Double)]
  }

  object encodeJsonKey {
    final case class Foo(value: String)
    object Foo {
      implicit val instance: EncodeJsonKey[Foo] = EncodeJsonKey.from(_.value)
    }

    EncodeJson.of[Map[Foo, Int]]
  }

  object derived {
    implicit def ProductEncodeJson: EncodeJson[Product] = EncodeJson.derive[Product]
    implicit def OrderLineEncodeJson: EncodeJson[OrderLine] = EncodeJson.derive[OrderLine]
    implicit def OrderEncodeJson: EncodeJson[Order] = EncodeJson.derive[Order]
    implicit def PersonEncodeJson: EncodeJson[Person] = EncodeJson.derive[Person]

    EncodeJson.of[Person]

    implicit def BackTicksEncodeJson: EncodeJson[BackTicks] = EncodeJson.derive[BackTicks]
    def testBackTicksEncodeJson = BackTicks("test").jencode === ("a.b.c" := "test") ->: jEmptyObject
  }
}
