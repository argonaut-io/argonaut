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

  // FIX unicode escaped chars? don't think they are being generated at the moment
  // FIX break this property apart, testing a few things at the moment.
  property("all that encodes can be decoded") =
          forAll({(j: Json) =>
              val pretty = p(subject.jvalue, JsonPrinter.pretty(j))
              val compact = p(subject.jvalue, JsonPrinter.compact(j))
              pretty.successful && pretty.get == j && compact.successful && compact.get == j
            })

  property("known json decodes") = 
          forAll({(c: CannedData) =>
              p(subject.jvalue, c.s).successful
            })

  property("known json, re-encodes") =
          forAll({(c: CannedData) =>
              val first = p(subject.jvalue, c.s).get
              val second = p(subject.jvalue, JsonPrinter.pretty(first)).get
              first == second && JsonPrinter.pretty(first) == JsonPrinter.pretty(second)
            })


  def p(k: subject.Parser[Json], s: String) =
    k(new CharSequenceReader(s))
}
