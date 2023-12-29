package argonaut

import scala.collection.mutable
import org.typelevel.jawn.Facade
import org.typelevel.jawn.FContext
import org.typelevel.jawn.SupportParser

object JawnParser extends SupportParser[Json] {
  implicit val facade: Facade[Json] =
    new Facade.NoIndexFacade[Json] {
      def jnull = Json.jNull
      def jfalse = Json.jFalse
      def jtrue = Json.jTrue
      def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = Json.jNumber(JsonNumber.unsafeDecimal(s.toString))
      def jstring(s: CharSequence) = Json.jString(s.toString)

      def singleContext(): FContext[Json] = new FContext.NoIndexFContext[Json] {
        var value: Json = null
        def add(s: CharSequence) = { value = jstring(s.toString) }
        def add(v: Json) = { value = v }
        def finish(): Json = value
        def isObj: Boolean = false
      }

      def arrayContext(): FContext[Json] = new FContext.NoIndexFContext[Json] {
        val vs = mutable.ListBuffer.empty[Json]
        def add(s: CharSequence) = { vs += jstring(s.toString) }
        def add(v: Json) = { vs += v }
        def finish(): Json = Json.jArray(vs.toList)
        def isObj: Boolean = false
      }

      def objectContext(): FContext[Json] = new FContext.NoIndexFContext[Json] {
        var key: String = null
        var vs = JsonObject.empty
        def add(s: CharSequence): Unit =
          if (key == null) { key = s.toString }
          else { vs = vs + (key, jstring(s.toString)); key = null }
        def add(v: Json): Unit = { vs = vs + (key, v); key = null }
        def finish() = Json.jObject(vs)
        def isObj = true
      }
    }
}
