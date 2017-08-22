package argonaut

import argonaut.Data._
import argonaut.JsonObjectCats._
import cats.syntax.show._

object JsonObjectCatsSpecification extends ArgonautSpec {
  def is = s2"""
  JsonObjectCats
    shows  ${prop((o: JsonObject) => o.show === o.toString)}
   """
}
