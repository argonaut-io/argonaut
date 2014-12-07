package argonaut

import Data._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._, Scalaz._
import Argonaut._

object CodecNumberSpecification extends Specification with ScalaCheck {

  def is = s2"""
  Codec Numbers
    double that is not NaN or infinity encodes to number $double
    int always encodes to number                         $intToNumber
    long always encodes to number                        $longToNumber
  """

  def double =  prop { (xs: List[Double]) => xs.filter(x => !x.isNaN && !x.isInfinity).asJson.array.forall(_.forall(_.isNumber)) }

  def intToNumber = prop { (xs: List[Int]) => xs.asJson.array.forall(_.forall(_.isNumber)) }

  def longToNumber = prop { (xs: List[Long]) => xs.asJson.array.forall(_.forall(_.isNumber)) }

}
