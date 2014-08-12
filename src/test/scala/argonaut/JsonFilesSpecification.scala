package argonaut

import Data._
import org.scalacheck._
import org.specs2._, org.specs2.specification._, execute.Result
import org.specs2.matcher._
import scalaz._, Scalaz._
import Argonaut._
import java.io.File

object JsonFilesSpecification extends Specification with ScalaCheck {
  def find = new File(getClass.getResource("/data").toURI).listFiles.toList

  case class JsonFile(file: File)

  implicit def JsonFileArbitrary: Arbitrary[JsonFile] =
    Arbitrary(Gen.oneOf(find.map(JsonFile)))

  def is = s2""" Predefined files can print and get same result" ! ${propNoShrink(test)} """

  val test: JsonFile => Result =
    jsonfile => {
      val string = scala.io.Source.fromFile(jsonfile.file).mkString
      val parsed = string.parseOption
      val json = parsed.getOrElse(sys.error("could not parse json file [" + jsonfile + "]"))
      json.nospaces.parseOption must beSome(json)
      json.spaces2.parseOption must beSome(json)
      json.spaces4.parseOption must beSome(json)
    }
}
