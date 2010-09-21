package com.ephox.argonaut

import org.scalacheck.Prop._
import util.parsing.input.CharSequenceReader
import org.scalacheck.{Gen, Arbitrary, Properties}

object ParserTest extends Properties("Parser") {
 val subject = new JsonParser


  case class SometimesNullString(s: String) {
    override def toString = s
  }

  /*

  assertEquals(Json.jsonNull, parse("null"))

  assertEquals(Json.jsonObject(x, y), parse("{" + x + ": " + y + }") }



   */


  implicit val ArbitrarySometimesNullString: Arbitrary[SometimesNullString] =
    Arbitrary(Gen.frequency((1, Gen.value("null")),
                            (9, Arbitrary.arbitrary[String])) map (SometimesNullString(_)))


  property("null parses successfully") =
          forAll((s: SometimesNullString) => (s != SometimesNullString("null")) ==>
            !subject.xnull(new CharSequenceReader(s.s)).successful)
  /*
  property("Dylan is cunfuzzldeded") = forAll((x: JKey, y: JValue) => {
    val k = "{" + x.toJSONStringRepr + ": " + y.toJSONStringRepr + "}" // use String.format
    val p = subject.xobject(new CSR(k))
    p.key = x && p.value == y
  })
  */
}
