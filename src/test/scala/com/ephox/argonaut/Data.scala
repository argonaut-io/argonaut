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

    // FIX Would like to pump up the level of complex objects being generated, but it falls over sometimes.
    Arbitrary(frequency((10, n), (10, b), (10, m), (10, s), (1, a), (4, o)))
  }
  
  case class SometimesNullString(s: String) {
    override def toString = s
  }

  case class SometimesBoolString(s: String) {
    override def toString = s
  }

  implicit val ArbitrarySometimesNullString: Arbitrary[SometimesNullString] =
      Arbitrary(Gen.frequency((1, value("null")), (9, Arbitrary.arbitrary[String])) map (SometimesNullString(_)))

  implicit val ArbitrarySometimesBoolString: Arbitrary[SometimesBoolString] =
      Arbitrary(Gen.frequency((1, value("true")), (1, value("false")), (8, Arbitrary.arbitrary[String])) map (SometimesBoolString(_)))

  implicit val ArbitraryCannedData: Arbitrary[CannedData] = 
      Arbitrary(Gen.oneOf(value(CannedData.webradarQuery), value(CannedData.webradarReport), value(CannedData.bookmarks)))
}
