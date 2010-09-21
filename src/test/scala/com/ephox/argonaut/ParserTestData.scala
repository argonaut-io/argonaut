package com.ephox.argonaut

import org.scalacheck.{Gen, Arbitrary}

object ParserTestData {
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
