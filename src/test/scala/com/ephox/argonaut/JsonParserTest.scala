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

  property("all that encodes can be decoded") =
          forAll({(j: Json) =>
              val parsed = p(subject.jvalue, j.emit)
              parsed.successful && parsed.get == j
            })

  property("known json decodes") = 
          forall({(c: CannedData) =>
              p(subject.jvalue, c.s).successful
            })

  def p(k: subject.Parser[Json], s: String) =
    k(new CharSequenceReader(s))
}
