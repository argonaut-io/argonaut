package com.ephox
package argonaut

import org.scalacheck.Prop._
import util.parsing.input.CharSequenceReader
import org.scalacheck.Properties
import Data._
import Argonaut._
import scalaz._, Scalaz._

object JsonParserTest extends Properties("JsonParser") {
  val subject = new JsonParser

  property("null parses ok") =
          forAll((s: SometimesNullString) => (s == SometimesNullString("null")) == p(subject.jnull, s.s).successful)

  property("null parses to null") =
          forAll((s: SometimesNullString) => s != SometimesNullString("null") || p(subject.jnull, s.s).get.isNull)

  property("boolean parses ok") =
          forAll((s: SometimesBoolString) => (List("true", "false").contains(s.s) == p(subject.jboolean, s.s).successful))

  property("boolean parses to bool") =
          forAll((s: SometimesBoolString) => (List("true", "false").contains(s.s) ==> p(subject.jboolean, s.s).get.isBool))

  def p(k: subject.Parser[Json], s: String) =
    k(new CharSequenceReader(s))

}
