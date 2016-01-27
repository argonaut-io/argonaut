package argonaut

import cats._

object JsonCats extends JsonCatss {
}

trait JsonCatss {
  implicit val JsonInstances: Eq[Json] with Show[Json] = {
    new Eq[Json] with Show[Json] {
      def eqv(a1: Json, a2: Json) = {
        a1 match {
          case JNull => a2.isNull
          case JBool(b) => a2.bool contains b
          case JNumber(n) => a2.number contains n
          case JString(s) => a2.string contains s
          case JArray(a) => a2.array contains a
          case JObject(o) => a2.obj contains o
        }
      }

      override def show(a: Json): String = Show.fromToString.show(a)
    }
  }
}
