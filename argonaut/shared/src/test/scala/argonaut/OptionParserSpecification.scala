package argonaut

import argonaut.Argonaut._

object OptionParserSpecification extends ArgonautSpec{  def is = s2"""

  Option parsing when option is top level
     decoding empty string gives none                                    $emptyString
     decoding malformed document gives none                              $malformed
     regular decoding is ok                                              $regular

  Option parsing within a nested structure
     regular decoding of structure with option is ok                     $nestedWithNone
     regular decoding of structure with some is ok                       $nestedWithSome
     failing correctly when decoding nested document with wrong types    $nestedIncorrectType
     failing correctly when nested document cannot be parsed             $nestedInvalidDocument"""

  case class AnObject(c: String)
  case class OtherObject(a: Int, b: Option[AnObject])


  implicit val codec2: CodecJson[AnObject] = CodecJson.derive[AnObject]
  implicit val codec1: CodecJson[OtherObject] = CodecJson.derive[OtherObject]

  def emptyString = "".decodeEither[AnObject] must beLeft

  def malformed = """{"foo": 1}""".decodeEither[AnObject] must beLeft
  def regular = """{"c": "Hello"}""".decodeOption[AnObject] must beSome(AnObject("Hello"))

  def nestedWithNone = """{"a": 1}""".decodeOption[OtherObject] must beSome(OtherObject(1, None))
  def nestedWithSome = """{"a": 1, "b": {"c": "Hello"}}""".decodeOption[OtherObject] must beSome(OtherObject(1, Some(AnObject("Hello"))))
  def nestedIncorrectType = """{"a": 1, "b": {"c": 1}}""".decodeEither[OtherObject] must beLeft
  def nestedInvalidDocument = """{"a": 1, "b": {"c1": "1"}}""".decodeEither[OtherObject] must beLeft

}
