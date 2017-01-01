package argonaut

import argonaut.Data._
import argonaut.JsonObjectCats._
import cats.syntax.show._
import org.specs2._

object JsonObjectCatsSpecification extends Specification with ScalaCheck {
  def is = s2"""
  JsonObjectCats
    shows  ${prop((o: JsonObject) => o.show === o.toString)}
   """
}
