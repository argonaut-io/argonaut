package argonaut

import java.io.File

import argonaut.Argonaut._
import org.specs2._

object JsonFilesSpecification extends Specification with ScalaCheck {
  def find = new File("./argonaut/src/test/resources/data").listFiles.toList

  case class JsonFile(file: File)

  def is = s2"""
  Predefined files can print and get same result  $test
  """

  def test() = {
    find.map(testFile).reduce(_.append(_))
  }

  def testFile(jsonFile: File) = {
    val string = scala.io.Source.fromFile(jsonFile).mkString
    val parsed = string.parseOption
    val json = parsed.getOrElse(sys.error("could not parse json file [" + jsonFile + "]"))
    s2"""
    ${jsonFile.getName}
      With no spaces          ${json.nospaces.parseOption must beSome(json)}
      With 2 spaces           ${json.spaces2.parseOption must beSome(json)}
      With 4 spaces           ${json.spaces4.parseOption must beSome(json)}"""
  }
}
