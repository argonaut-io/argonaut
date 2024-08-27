package argonaut

import JsonNumberCats.*
import JsonObjectCats.*
import cats.*
import instances.list.*
import syntax.eq.*

object JsonCats extends JsonCatss {}

trait JsonCatss {
  implicit val JsonInstances: Eq[Json] & Show[Json] = {
    new Eq[Json] with Show[Json] {
      def eqv(a1: Json, a2: Json) = {
        a1 match {
          case JNull => a2.isNull
          case JBool(b) => a2.bool exists (_ == b)
          case JNumber(n) => a2.number exists (_ === n)
          case JString(s) => a2.string exists (_ == s)
          case JArray(a) => a2.array exists (_ === a)
          case JObject(o) => a2.obj exists (_ === o)
        }
      }

      override def show(a: Json): String = Show.fromToString.show(a)
    }
  }
}
