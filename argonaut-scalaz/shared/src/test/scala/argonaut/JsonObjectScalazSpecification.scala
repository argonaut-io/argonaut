package argonaut

import Data.*
import scalaz.*
import Scalaz.*
import JsonObjectScalaz.*

object JsonObjectScalazSpecification extends ArgonautSpec {
  def is = s2"""
  JsonObjectScalaz
    shows  ${prop((o: JsonObject) => o.shows === o.toString)}
   """
}
