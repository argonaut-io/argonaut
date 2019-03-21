package argonaut

import Data._
import scalaz._,Scalaz._
import JsonObjectScalaz._

object JsonObjectScalazSpecification extends ArgonautSpec {
  def is = s2"""
  JsonObjectScalaz
    shows  ${ prop((o: JsonObject) => o.shows === o.toString) }
   """
}
