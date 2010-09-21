package com.ephox.argonaut

import org.scalacheck.Prop._
import util.parsing.input.CharSequenceReader
import org.scalacheck.{Gen, Arbitrary, Properties}

object ParserTest extends Properties("Parser") {
  val subject = new JsonParser

  case class SometimesNullString(s: String) {
    override def toString = s
  }

  implicit val ArbitrarySometimesNullString: Arbitrary[SometimesNullString] =
    Arbitrary(Gen.frequency((1, Gen.value("null")),
                            (9, Arbitrary.arbitrary[String])) map (SometimesNullString(_)))


  property("null parses successfully") =
          forAll((s: SometimesNullString) => (s != SometimesNullString("null")) ==>
            !p(subject.xnull, s.s).successful)
  /*
  property("Dylan is cunfuzzldeded") = forAll((x: JKey, y: JValue) => {
    val k = "{" + x.toJSONStringRepr + ": " + y.toJSONStringRepr + "}" // use String.format
    val p = subject.xobject(new CSR(k))
    p.key = x && p.value == y
  })
  */


  def p(k: subject.Parser[Json], s: String) = {
    val r = new CharSequenceReader(s)
    k(r)
  }


}
