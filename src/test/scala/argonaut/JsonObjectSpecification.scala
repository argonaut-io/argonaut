package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import JsonIdentity._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

object JsonObjectSpecification extends Specification with ScalaCheck {
  def is = s2"""
  JsonObject
    fields ${ prop((o: JsonObject) => o.fields.length == o.fieldSet.size) }
    shows  ${ prop((o: JsonObject) => o.shows == o.toString) }
   """
}
