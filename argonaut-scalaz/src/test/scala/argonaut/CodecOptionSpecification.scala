package argonaut

import org.specs2._
import Argonaut._

object CodecOptionSpecification extends Specification with ScalaCheck {
  case class Thing(value: Option[String])

  implicit def ThingCodecJson: CodecJson[Thing] =
    casecodec1(Thing.apply, Thing.unapply)("value")

  def is = s2"""
  Codec Option
    handles missing field    ${jEmptyObject.as[Thing] must_== DecodeResult.ok(Thing(None))}
    handles null field       ${Json.obj("value" := jNull).as[Thing] must_== DecodeResult.ok(Thing(None))}
    handles set field                            $setField
    handles missing nested fields using as[T]    $missingNestedAs
    handles missing nested fields using get[T]   $missingNestedGet
  """

  def setField = prop { (value: String) => Json.obj("value" := value).as[Thing] must_== DecodeResult.ok(Thing(Some(value))) }

  def missingNestedAs = prop { (value: String) =>
    val third = Json.obj("first" := jEmptyObject)
      .hcursor
      .downField("first")
      .downField("second")
      .downField("third")
      .as[Option[String]]
    third must_== DecodeResult.ok(None)
  }

  def missingNestedGet = prop { (value: String) =>
    val third = Json.obj("first" := jEmptyObject)
      .hcursor
      .downField("first")
      .downField("second")
      .get[Option[String]]("third")
    third must_== DecodeResult.ok(None)
  }
}
