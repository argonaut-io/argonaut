package argonaut

import Data._
import org.specs2._
import scalaz._,Scalaz._
import JsonObjectScalaz._

object JsonObjectScalazSpecification extends Specification with ScalaCheck {
  def is = s2"""
  JsonObjectScalaz
    shows  ${ prop((o: JsonObject) => o.shows === o.toString) }
   """
}
