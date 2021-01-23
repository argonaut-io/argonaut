package argonaut

import java.io.File
import scala.io.Source

object TestMain {

  def main(args: Array[String]): Unit = {
    val base = new File("argonaut/jvm/src/test/resources/data/")
    val jsonFiles = base.listFiles.filter(_.getName.endsWith(".json")).sortBy(_.length)

    jsonFiles.foreach { jsonFile =>
      val time0 = System.currentTimeMillis
      val json = Source.fromFile(jsonFile).getLines().mkString("\n")
      val time1 = System.currentTimeMillis
      JsonParser.parse(json) match {
        case Right(j) =>
          assert(JsonParser.parse(j.spaces2) == Right(j))
          println(s"${jsonFile} ${System.currentTimeMillis() - time1} ${time1 - time0}")
        case Left(e) =>
          sys.error(e)
      }
    }
  }
}
