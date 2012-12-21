package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

object CodecSpecification extends Specification with ScalaCheck {
  def encodedecode[A: DecodeJson : EncodeJson : Arbitrary : Equal] =
    prop((a: A) =>
      implicitly[DecodeJson[A]].apply(implicitly[EncodeJson[A]].apply(a).hcursor).value exists (_ === a)
    )

  def is = "Codec" ^
    "Unit encode/decode" ! encodedecode[Unit] ^
    "List[String] encode/decode" ! encodedecode[List[String]] ^
    "Stream[String] encode/decode" ! encodedecode[Stream[String]] ^
    "String encode/decode" ! encodedecode[String] ^
    "Double encode/decode" ! encodedecode[Double] ^
    "Float encode/decode" ! encodedecode[Float] ^
    "Int encode/decode" ! encodedecode[Int] ^
    "Long encode/decode" ! encodedecode[Long] ^
    "Boolean encode/decode" ! encodedecode[Boolean] ^
    "Char encode/decode" ! encodedecode[Char] ^
    "Option[String] encode/decode" ! encodedecode[Option[String]] ^
    "Either[String, Int] encode/decode" ! encodedecode[Either[String, Int]] ^
    "String \\/ Int encode/decode" ! encodedecode[String \/ Int] ^
    "Map[String, Int] encode/decode" ! encodedecode[Map[String, Int]] ^
    "Set[String] encode/decode" ! encodedecode[Set[String]] ^
    "Tuple2[String, Int] encode/decode" ! encodedecode[Tuple2[String, Int]] ^
    "Tuple3[String, Int, Boolean] encode/decode" ! encodedecode[Tuple3[String, Int, Boolean]] ^
    "Tuple4[String, Int, Boolean, Long] encode/decode" ! encodedecode[Tuple4[String, Int, Boolean, Long]] ^ end
}
