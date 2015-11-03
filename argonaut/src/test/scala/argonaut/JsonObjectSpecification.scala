package argonaut

import Data._
import org.specs2._
import scalaz.syntax.show._

object JsonObjectSpecification extends Specification with ScalaCheck {
  def is = s2"""
  JsonObject
    fields ${ prop((o: JsonObject) => o.fields.length == o.fieldSet.size) }
   """
}
