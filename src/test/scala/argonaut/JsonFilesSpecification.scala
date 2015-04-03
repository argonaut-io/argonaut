package argonaut

import java.io.File

import argonaut.Argonaut._
import org.scalacheck.Prop._
import org.scalacheck._
import org.specs2._

object JsonFilesSpecification extends Specification with ScalaCheck {
  def find = new File(getClass.getResource("/data").toURI).listFiles.toList

  case class JsonFile(file: File)

  implicit def JsonFileArbitrary: Arbitrary[JsonFile] =
    Arbitrary(Gen.oneOf(find.map(JsonFile)))

  def is = s2""" Predefined files can print and get same result" ! $test """

  val test = forAllNoShrink { jsonfile: JsonFile =>
      val string = scala.io.Source.fromFile(jsonfile.file).mkString
      val parsed = string.parseOption
      val json = parsed.getOrElse(sys.error("could not parse json file [" + jsonfile + "]"))
      json.nospaces.parseOption must beSome(json)
      json.spaces2.parseOption must beSome(json)
      json.spaces4.parseOption must beSome(json)
    }
}
