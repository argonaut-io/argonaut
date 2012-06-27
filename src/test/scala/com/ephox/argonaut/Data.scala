package com.ephox
package argonaut

import scalaz._, Scalaz._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{frequency, choose, listOfN, value, oneOf}
import Json._
import org.scalacheck.{Gen, Arbitrary}

object Data {
  implicit def ArbitraryJsonNumber: Arbitrary[JsonNumber] =
    Arbitrary(arbitrary[Double] map (JsonNumber(_)))

  implicit def ArbitraryJson: Arbitrary[Json] = {
    val n = value(jNull)
    val b = arbitrary[Boolean] map (jBool(_))
    val m = arbitrary[JsonNumber] map (jNumber(_))
    val s = arbitrary[String] map (jString(_))
    val a = for(n <- choose(0, 10);
                j <- listOfN(n, arbitrary[Json]))
            yield jArray(j)
    val o = for(n <- choose(0, 10);
                j <- listOfN(n, arbitrary[(String, Json)]))
            yield jObjectAssocList(j)

    // FIX Would like to pump up the level of complex objects being generated, but it falls over sometimes.
    Arbitrary(frequency((10, n), (10, b), (10, m), (10, s), (1, a), (4, o)))
  }

  implicit def ArbitraryJsonObject: Arbitrary[JsonObject] =
    Arbitrary(arbitrary[List[(JsonField, Json)]] map (as => JsonObject(InsertionMap(as: _*))))

  implicit def ArbitraryCursor: Arbitrary[Cursor] = {
    Arbitrary(arbitrary[Json] flatMap (j => {
      val c = +j
      j.arrayOrObject(
        Gen.value(c)
      , _ =>
          for {
            r <- frequency((90, arbitrary[Cursor]), (10, c))
          } yield c.right getOrElse r
      , o =>
          for {
            r <- frequency((90, arbitrary[Cursor]), (10, c))
            q <- frequency((90, oneOf(o.fields)), (10, arbitrary[JsonField]))
          } yield c downField q getOrElse r
      )
    }))
  }

  case class SometimesNullString(s: String) {
    override def toString = s
  }

  case class SometimesBoolString(s: String) {
    override def toString = s
  }

  implicit val ArbitrarySometimesNullString: Arbitrary[SometimesNullString] =
      Arbitrary(frequency((1, value("null")), (9, arbitrary[String])) map (SometimesNullString(_)))

  implicit val ArbitrarySometimesBoolString: Arbitrary[SometimesBoolString] =
      Arbitrary(frequency((1, value("true")), (1, value("false")), (8, arbitrary[String])) map (SometimesBoolString(_)))
}
