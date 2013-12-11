package argonaut

import Data._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._, Scalaz._
import Argonaut._


object CodecOptionSpecification extends Specification with ScalaCheck {
  case class Thing(value: Option[String])

  implicit def ThingCodecJson: CodecJson[Thing] =
    casecodec1(Thing.apply, Thing.unapply)("value")

  def is = "Codec Option" ^
    "handles missing field" ! { jEmptyObject.as[Thing] must_== DecodeResult.ok(Thing(None)) } ^
    "handles null field" ! { Json.obj("value" := jNull).as[Thing] must_== DecodeResult.ok(Thing(None)) } ^
    "handles set field" ! prop { (value: String) => Json.obj("value" := value).as[Thing] must_== DecodeResult.ok(Thing(Some(value))) } ^
    "handles missing nested fields using as[T]" ! prop { (value: String) =>
      val third = Json.obj("first" := jEmptyObject)
        .hcursor
        .downField("first")
        .downField("second")
        .downField("third")
        .as[Option[String]]
      third must_== DecodeResult.ok(None)
    } ^
    "handles missing nested fields using get[T]" ! prop { (value: String) =>
      val third = Json.obj("first" := jEmptyObject)
        .hcursor
        .downField("first")
        .downField("second")
        .get[Option[String]]("third")
      third must_== DecodeResult.ok(None)
    } ^
    end
}
