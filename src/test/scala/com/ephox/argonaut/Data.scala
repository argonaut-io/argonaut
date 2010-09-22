package com.ephox.argonaut

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{frequency, choose, listOfN, value}
import Json._
import org.scalacheck.{Gen, Arbitrary}

object Data {
  implicit def ArbitraryJson: Arbitrary[Json] = {
    val n = value(jsonNull)
    val b = arbitrary[Boolean] map (jsonBool(_))
    val m = arbitrary[JsonNumber] map (jsonNumber(_))
    val s = arbitrary[String] map (jsonString(_))
    val a = for(n <- choose(0, 10);
                j <- listOfN(n, arbitrary[Json]))
            yield jsonArray(j)
    val o = for(n <- choose(0, 10);
                j <- listOfN(n, arbitrary[(String, Json)]))
            yield jsonObject(j)

    Arbitrary(frequency((10, n), (10, b), (10, m), (10, s), (1, a), (1, o)))
  }
  
  case class SometimesNullString(s: String) {
    override def toString = s
  }

  case class SometimesBoolString(s: String) {
    override def toString = s
  }

  implicit val ArbitrarySometimesNullString: Arbitrary[SometimesNullString] =
      Arbitrary(Gen.frequency((1, Gen.value("null")), (9, Arbitrary.arbitrary[String])) map (SometimesNullString(_)))

  implicit val ArbitrarySometimesBoolString: Arbitrary[SometimesBoolString] =
      Arbitrary(Gen.frequency((1, Gen.value("true")), (1, Gen.value("false")), (8, Arbitrary.arbitrary[String])) map (SometimesBoolString(_)))
}