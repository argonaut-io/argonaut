package argonaut

import java.io.{BufferedInputStream, File, FileInputStream}

object TestMain {

  // scala.io.Source is too slow in scala-native 0.2.1
  private[this] def read(file: File): String = {
    val in = new BufferedInputStream(new FileInputStream(file))
    try {
      val contents = new Array[Byte](4096)
      var n = 0
      var str = ""
      while ({ n = in.read(contents); n } != -1) {
        str += new String(contents, 0, n)
      }
      str
    } finally {
      in.close()
    }
  }

  def main(args: Array[String]): Unit = {
    val base = new File("argonaut/jvm/src/test/resources/data/")
    val jsonFiles = base.listFiles.filter(_.getName.endsWith(".json")).sortBy(_.length)

    jsonFiles.foreach { jsonFile =>
      val time0 = System.currentTimeMillis
      val json = read(jsonFile)
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
