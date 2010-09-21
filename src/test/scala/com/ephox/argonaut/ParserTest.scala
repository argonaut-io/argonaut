package com.ephox.argonaut

import org.scalacheck.Prop._
import util.parsing.input.CharSequenceReader
import org.scalacheck.{Gen, Arbitrary, Properties}
import Json._
import ParserTestData._

object ParserTest extends Properties("Parser") {
  val subject = new JsonParser

  def ifItPassesIt(parser: subject.Parser[Json], s: String, expectedValue: Json) = {
    val r = p(parser, s)
    r.successful && r.getOrElse("dummydefaultvalue") == expectedValue
  }


  property("null parses ok") =
          forAll((s: SometimesNullString) => (s == SometimesNullString("null")) == p(subject.xnull, s.s).successful)

  property("null parses to null") =
          forAll((s: SometimesNullString) => (s == SometimesNullString("null")) ==> p(subject.xnull, s.s).get.isNull)

  property("boolean parses ok") =
          forAll((s: SometimesBoolString) => (List("true", "false").contains(s.s) == p(subject.xboolean, s.s).successful))

  property("boolean parses to bool") =
          forAll((s: SometimesBoolString) => (List("true", "false").contains(s.s) ==> p(subject.xboolean, s.s).get.isBool))
  
  def p(k: subject.Parser[Json], s: String) = {
    val r = new CharSequenceReader(s)
    k(r)
  }


}
