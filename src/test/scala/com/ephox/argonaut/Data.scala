package com.ephox.argonaut

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{frequency, choose, listOfN}
import Json._
import org.scalacheck.Arbitrary

object Data {
  implicit def ArbitraryJson: Arbitrary[Json] = {
    val n = arbitrary[Unit] map (_ => jsonNull)
    val b = arbitrary[Boolean] map (jsonBool(_))
    val m = arbitrary[JsonNumber] map (jsonNumber(_))
    val s = arbitrary[String] map (jsonString(_))
    val a =  for(n <- choose(0, 10);
                 j <- listOfN(n, arbitrary[Json]))
             yield jsonArray(j)
    val o = for(n <- choose(0, 10);
                 j <- listOfN(n, arbitrary[(String, Json)]))
             yield jsonObject(j)

    Arbitrary(frequency((10, n), (10, b), (10, m), (10, s), (1, a), (1, o)))
  }
}