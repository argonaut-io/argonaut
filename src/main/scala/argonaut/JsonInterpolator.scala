package argonaut

import scalaz.\/

trait JsonInterpolator {

  implicit class JsonHelper(sc: StringContext) {
    def json(args: Any*): String \/ Json = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      Parse.parse(buf.toString)
    }
  }

}