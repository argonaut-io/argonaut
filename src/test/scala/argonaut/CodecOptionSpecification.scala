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
    end

}
