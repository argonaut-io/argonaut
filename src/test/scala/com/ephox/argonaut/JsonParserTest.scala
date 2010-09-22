package com.ephox.argonaut

import org.scalacheck.Prop._
import util.parsing.input.CharSequenceReader
import org.scalacheck.Properties
import Data._

object JsonParserTest extends Properties("Parser") {
  val subject = new JsonParser

  property("null parses ok") =
          forAll((s: SometimesNullString) => (s == SometimesNullString("null")) == p(subject.jnull, s.s).successful)

  property("null parses to null") =
          forAll((s: SometimesNullString) => s != SometimesNullString("null") || p(subject.jnull, s.s).get.isNull)

  property("boolean parses ok") =
          forAll((s: SometimesBoolString) => (List("true", "false").contains(s.s) == p(subject.jboolean, s.s).successful))

  property("boolean parses to bool") =
          forAll((s: SometimesBoolString) => (List("true", "false").contains(s.s) ==> p(subject.jboolean, s.s).get.isBool))

  // FIX Does not handle spaces yet.
  // FIX Emit is naive. Not a good enough test at the moment.
  // FIX escaped chars?
  property("all that encodes can be decodes") =
          forAll({(j: Json) =>
              val parsed = p(subject.jvalue, j.emit)
              parsed.successful && parsed.get == j
            })

  // FIX flush out other properties.

  def p(k: subject.Parser[Json], s: String) =
    k(new CharSequenceReader(s))
}
