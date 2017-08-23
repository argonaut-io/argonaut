package argonaut

import Data._

object JsonObjectSpecification extends ArgonautSpec {
  def is = s2"""
  JsonObject
    fields ${ prop((o: JsonObject) => o.fields.length == o.fieldSet.size) }
   """
}
