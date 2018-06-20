package argonaut

import argonaut.Argonaut._
import org.specs2.specification.core.Fragments
import JsonFilesSpecBase.TestData

object JsonFilesSpecBase {
  final case class TestData(fileName: String, jsonString: String)
}

abstract class JsonFilesSpecBase extends ArgonautSpec {
  def is = s2"""
  Predefined files can print and get same result  $test
  """

  val TestData = JsonFilesSpecBase.TestData

  def testData: collection.Seq[TestData]

  def test: Fragments =
    testData.map(testFile).reduce(_.append(_))

  val baseDir = "./argonaut/jvm/src/test/resources/data"

  def testFile(t: TestData) = {
    val parsed = t.jsonString.parseOption
    val json = parsed.getOrElse(sys.error("could not parse json file [" + t.fileName + "]"))
    s2"""
    ${t.fileName}
      With no spaces          ${json.nospaces.parseOption must beSome(json)}
      With 2 spaces           ${json.spaces2.parseOption must beSome(json)}
      With 4 spaces           ${json.spaces4.parseOption must beSome(json)}"""
  }
}
