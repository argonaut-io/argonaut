package argonaut

import argonaut.Argonaut._
import org.specs2.Specification

class TooLenientOptionParserSpecification extends Specification{  def is = s2"""

  Option parsing works great on base types
     obviously decoding empty string gives option                       $emptyStringOk
     obvously decoding malformed document gives none                    $malformedOk
     just checking, regular decoding is ok                              $regularOk

  Neste option types is problematic when nested type does not parse
     just checking, regular decoding of structure with option is ok     $nestedWithNoneOk
     just checking, regular decoding of structure with some is ok       $nestedWithSomeOk
     fails nested decoding when nested document has wrong types         $nestedIncorrectTypeOk
     wrongly accepts nested object as None when it cannot be parsed     $nestedParsedToNoneNotOk"""

  case class AnObject(c: String)
  case class OtherObject(a: Int, b: Option[AnObject])


  implicit val codec2 = CodecJson.derive[AnObject]
  implicit val codec1 = CodecJson.derive[OtherObject]

  def emptyStringOk = "".decodeEither[AnObject] must beLeft

  def malformedOk = """{"foo": 1}""".decodeEither[AnObject] must beLeft
  def regularOk = """{"c": "Hello"}""".decodeOption[AnObject] must beSome(AnObject("Hello"))

  def nestedWithNoneOk = """{"a": 1}""".decodeOption[OtherObject] must beSome(OtherObject(1, None))
  def nestedWithSomeOk = """{"a": 1, "b": {"c": "Hello"}}""".decodeOption[OtherObject] must beSome(OtherObject(1, Some(AnObject("Hello"))))
  def nestedIncorrectTypeOk = """{"a": 1, "b": {"c": 1}}""".decodeEither[OtherObject] must beLeft
  def nestedParsedToNoneNotOk = """{"a": 1, "b": {"c1": "1"}}""".decodeEither[OtherObject] must beLeft

}
