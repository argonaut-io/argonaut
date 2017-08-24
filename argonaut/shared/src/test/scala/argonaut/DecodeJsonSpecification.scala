package argonaut

import Argonaut._

object DecodeJsonSpecification extends ArgonautSpec { def is = s2"""
  DecodeJson flatMapCursor
    Successful flatMap                    ${successfulFlatMapCursor}
    Failing flatMap                       ${failingFlatMapCursor}
  DecodeJson Witness Compilation
    Witness basics                        ${ok}
    Witness tuples                        ${ok}
    Witness auto                          ${ok}
    Witness derived                       ${ok}
  DecodeJson derive
    BackTicks                             ${derived.testBackTicksDecodeJson}
"""

  def successfulFlatMapCursor = {
    prop{(key: String, n: Int) =>
      val json = (key, n.jencode) ->: jEmptyObject
      val decodeJson = DecodeJson.of[Int].flatMapCursor(_.get[HCursor](key))
      decodeJson.decodeJson(json) must beEqualTo(DecodeResult.ok(n))
    }
  }

  def failingFlatMapCursor = {
    prop{(key: String, n: Int) =>
      val json = (key, n.jencode) ->: jEmptyObject
      val decodeJson = DecodeJson.of[Int].flatMapCursor(_.get[HCursor]("TOTALLYNOTLIKELYTOBERANDOMLYGENERATEDTEXT"))
      val failure = DecodeResult.fail("Attempt to decode value on failed cursor.", CursorOp.failedOp(CursorOpDownField("TOTALLYNOTLIKELYTOBERANDOMLYGENERATEDTEXT")) +: CursorHistory.empty)
      decodeJson.decodeJson(json) must beEqualTo(failure)
    }
  }

  object primitives {
    DecodeJson.of[String]
    DecodeJson.of[Int]
    DecodeJson.of[Boolean]
    DecodeJson.of[Long]
    DecodeJson.of[Double]
    DecodeJson.of[Short]
    DecodeJson.of[Byte]
    DecodeJson.of[Option[Int]]
    DecodeJson.of[Option[String]]
  }

  object tuples {
    DecodeJson.of[(String, Int)]
    DecodeJson.of[(String, Int, Boolean)]
    DecodeJson.of[(String, Int, Boolean, Long)]
    DecodeJson.of[(String, Int, Boolean, Long, Double)]
  }

  object derived {

    implicit def ShapeDecodeJson: DecodeJson[Shape] = DecodeJson.derive[Circle] ||| DecodeJson.derive[Square]
    DecodeJson.of[Shape]

    implicit def ProductDecodeJson: DecodeJson[Product] = DecodeJson.derive[Product]
    implicit def OrderLineDecodeJson: DecodeJson[OrderLine] = DecodeJson.derive[OrderLine]
    implicit def OrderDecodeJson: DecodeJson[Order] = DecodeJson.derive[Order]
    implicit def PersonDecodeJson: DecodeJson[Person] = DecodeJson.derive[Person]

    DecodeJson.of[Person]

    implicit def BackTicksDecodeJson: DecodeJson[BackTicks] = DecodeJson.derive[BackTicks]
    def testBackTicksDecodeJson = Parse.decodeEither[BackTicks]("""{"a.b.c": "test"}""") == Right(BackTicks("test"))
  }
}
