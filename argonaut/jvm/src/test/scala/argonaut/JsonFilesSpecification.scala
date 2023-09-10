package argonaut

import argonaut.JsonFilesSpecBase.TestData
import java.io.File
import scala.io.Source

object JsonFilesSpecification extends JsonFilesSpecBase {
  override def testData: Seq[TestData] = {
    val files = new File(baseDir).listFiles.toList
    files.map{ file =>
      TestData(
        fileName = file.getName,
        jsonString = Source.fromFile(file).mkString
      )
    }
  }
}
