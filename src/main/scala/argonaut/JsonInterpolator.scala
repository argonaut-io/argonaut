package argonaut

import scalaz.\/

trait JsonInterpolator {

  implicit class JsonHelper(sc: StringContext) {
    def json(expressions: Any*): String \/ Json = {
      val stringsItr = sc.parts.iterator
      val expressionsItr = expressions.iterator
      
      // Combine string parts, starting with fencepost
      var buffer = new StringBuffer(stringsItr.next)
      while (stringsItr.hasNext) {
        buffer.append(expressionsItr.next)
        buffer.append(stringsItr.next)
      }
      Parse.parse(buffer.toString)
    }
  }

}