package argonaut

import Data._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._, Scalaz._
import Argonaut._

object CodecNumberSpecification extends Specification with ScalaCheck {

  def is = "Codec Numbers" ^
    "double always encodes to number" ! prop { (xs: List[Double]) => xs.asJson.array.forall(_.forall(_.isNumber)) } ^
    "int always encodes to number" ! prop { (xs: List[Int]) => xs.asJson.array.forall(_.forall(_.isNumber)) } ^
    "long always encodes to string" ! prop { (xs: List[Long]) => xs.asJson.array.forall(_.forall(_.isString)) } ^
    end

}
