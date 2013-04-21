package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import JsonIdentity._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

object CodecSpecification extends Specification with ScalaCheck {
  def encodedecode[A: DecodeJson : EncodeJson : Arbitrary : Equal] =
    prop((a: A) =>
      implicitly[DecodeJson[A]].apply(implicitly[EncodeJson[A]].apply(a).hcursor).value exists (_ === a)
    )

  case class TestClass(a: Int, b: Int, c: String, d: Int, e: Int, f: String, g: Int, h: Int, i: String, j: Int, k: Int, l: String, m: Int, n: Int, o: String, p: Int, q: Int, r: String, s: Int, t: Int, u: String, v: Boolean)

  implicit val arbTestClass: Arbitrary[TestClass] = Arbitrary(for {
    a <- arbitrary[Int]
    b <- arbitrary[Int]
    c <- arbitrary[String]
    d <- arbitrary[Int]
    e <- arbitrary[Int]
    f <- arbitrary[String]
    g <- arbitrary[Int]
    h <- arbitrary[Int]
    i <- arbitrary[String]
    j <- arbitrary[Int]
    k <- arbitrary[Int]
    l <- arbitrary[String]
    m <- arbitrary[Int]
    n <- arbitrary[Int]
    o <- arbitrary[String]
    p <- arbitrary[Int]
    q <- arbitrary[Int]
    r <- arbitrary[String]
    s <- arbitrary[Int]
    t <- arbitrary[Int]
    u <- arbitrary[String]
    v <- arbitrary[Boolean]
  } yield TestClass(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))

  implicit val equalTestClass: Equal[TestClass] = Equal.equalA

  implicit val showTestClass: Show[TestClass] = Show.showFromToString

  implicit val testClassEncode: EncodeJson[TestClass] = EncodeJson.jencode22L((x: TestClass) => (x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h, x.i, x.j, x.k, x.l, x.m, x.n, x.o, x.p, x.q, x.r, x.s, x.t, x.u, x.v))("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")

  implicit val testClassDecode: DecodeJson[TestClass] = DecodeJson.jdecode22L(TestClass.apply)("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")

  def encodeL22DecodeL22(): Prop = {
    prop{testClass: TestClass =>
      val encodedValue = testClass.jencode
      ("encoded value = " + encodedValue.shows) |: {
        val decodeResult = encodedValue.jdecode[TestClass]
        ("decode result = " + decodeResult.shows) |: {
          decodeResult === DecodeResult.ok(testClass)
        }
      }
    }
  }

  def is = "Codec" ^
    "Unit encode/decode" ! encodedecode[Unit] ^
    "List[String] encode/decode" ! encodedecode[List[String]] ^
    "Vector[String] encode/decode" ! encodedecode[Vector[String]] ^
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
    "Tuple4[String, Int, Boolean, Long] encode/decode" ! encodedecode[Tuple4[String, Int, Boolean, Long]] ^
    "22 field class encode/decode" ! encodeL22DecodeL22 ^ end
}
