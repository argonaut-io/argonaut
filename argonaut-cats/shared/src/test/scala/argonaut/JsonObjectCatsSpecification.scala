package argonaut

import argonaut.Data.*
import argonaut.JsonObjectCats.*
import cats.syntax.show.*

object JsonObjectCatsSpecification extends ArgonautSpec {
  def is = s2"""
  JsonObjectCats
    shows  ${prop((o: JsonObject) => o.show === o.toString)}
   """
}
