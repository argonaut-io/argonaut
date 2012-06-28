package com.ephox
package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import scalaz._, Scalaz._

object CodecTest extends Properties("Codec") {
  def encodedecode[A: DecodeJson : EncodeJson : Arbitrary : Equal] =
    forAll((a: A) =>
      implicitly[DecodeJson[A]].apply(implicitly[EncodeJson[A]].apply(a)).value exists (_ === a)
    )

  property("List[String] encode/decode") =
    encodedecode[List[String]]

  property("Stream[String] encode/decode") =
    encodedecode[Stream[String]]

  property("String encode/decode") =
    encodedecode[String]

  property("Double encode/decode") =
    encodedecode[Double]

  property("Float encode/decode") =
    encodedecode[Float]

  property("Int encode/decode") =
    encodedecode[Int]

  property("Long encode/decode") =
    encodedecode[Long]

  property("Boolean encode/decode") =
    encodedecode[Boolean]

  property("Char encode/decode") =
    encodedecode[Char]

  property("Option[String] encode/decode") =
    encodedecode[Option[String]]

  property("Either[String, Int] encode/decode") =
    encodedecode[Either[String, Int]]

  property("Map[String, Int] encode/decode") =
    encodedecode[Map[String, Int]]

  property("Set[String] encode/decode") =
    encodedecode[Set[String]]

  property("Tuple2[String, Int] encode/decode") =
    encodedecode[Tuple2[String, Int]]

  property("Tuple3[String, Int, Boolean] encode/decode") =
    encodedecode[Tuple3[String, Int, Boolean]]

}
