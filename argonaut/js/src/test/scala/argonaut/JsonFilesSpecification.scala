package argonaut

import scalajs.js
import scalajs.js.Dynamic

object JsonFilesSpecification extends JsonFilesSpecBase {
  val fs = Dynamic.global.require("fs")

  def listFiles(path: String): collection.Seq[String] =
    fs.readdirSync(path).asInstanceOf[js.Array[String]]

  override def testData = {
    listFiles(baseDir).map(path =>
      TestData(
        fileName = path.split('/').last,
        jsonString = fs.readFileSync(baseDir + "/" + path).toString
      )
    )
  }
}
