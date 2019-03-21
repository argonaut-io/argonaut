package argonaut

import Data._
import org.scalacheck._, Arbitrary._

object CodecSpecification extends ArgonautSpec {
  def encodedecode[A: EncodeJson: DecodeJson: Arbitrary] = {
    val aCodec = CodecJson.derived[A]
    prop[A, Boolean]{a =>
      CodecJson.codecLaw(aCodec)(a)
    }
  }

  def is = s2"""
  Codec
    Unit encode/decode ${encodedecode[Unit]}
    Json encode/decode ${encodedecode[Json]}
    UUID encode/decode ${encodedecode[java.util.UUID]}
    List[String] encode/decode ${encodedecode[List[String]]}
    List[Int] encode/decode ${encodedecode[List[Int]]}
    List[Long] encode/decode ${encodedecode[List[Long]]}
    List[Double] encode/decode ${encodedecode[List[Double]]}
    Vector[String] encode/decode ${encodedecode[Vector[String]]}
    Stream[String] encode/decode ${encodedecode[Stream[String]]}
    String encode/decode ${encodedecode[String]}
    Double encode/decode ${encodedecode[Double]}
    Float encode/decode ${encodedecode[Float]}
    Int encode/decode ${encodedecode[Int]}
    Long encode/decode ${encodedecode[Long]}
    Short encode/decode ${encodedecode[Short]}
    BigInt encode/decode ${encodedecode[BigInt]}
    BigDecimal encode/decode ${encodedecode[BigDecimal]}
    Boolean encode/decode ${encodedecode[Boolean]}
    Char encode/decode ${encodedecode[Char]}
    java.lang.Double encode/decode ${encodedecode[java.lang.Double]}
    java.lang.Float encode/decode ${encodedecode[java.lang.Float]}
    java.lang.Integer encode/decode ${encodedecode[java.lang.Integer]}
    java.lang.Long encode/decode ${encodedecode[java.lang.Long]}
    java.lang.Short encode/decode ${encodedecode[java.lang.Short]}
    java.lang.Boolean encode/decode ${encodedecode[java.lang.Boolean]}
    java.lang.Character encode/decode ${encodedecode[java.lang.Character]}
    Option[String] encode/decode ${encodedecode[Option[String]]}
    Either[String, Int] encode/decode ${encodedecode[Either[String, Int]]}
    Map[String, Int] encode/decode ${encodedecode[Map[String, Int]]}
    Set[String] encode/decode ${encodedecode[Set[String]]}
    Tuple2[String, Int] encode/decode ${encodedecode[Tuple2[String, Int]]}
    Tuple3[String, Int, Boolean] encode/decode ${encodedecode[Tuple3[String, Int, Boolean]]}
    Tuple4[String, Int, Boolean, Long] encode/decode ${encodedecode[Tuple4[String, Int, Boolean, Long]]}
    22 field class with codec ${import CodecInstances._; encodedecode[TestClass]}
    22 field class with codec derived ${import EncodeDecodeInstances._; encodedecode[TestClass]}
    CodecJson[Person] derived ${derived.testDerivedPerson}
    CodecJson[BackTicks] derived ${derived.testDerivedBackTicks}
    CodecJson[Shape] derived ${derived.testDerivedShape}
  """

  def mapArbitrary[A, B](arbitrary: Arbitrary[A])(f: A => B): Arbitrary[B] = {
    Arbitrary(arbitrary.arbitrary.map(f))
  }

  implicit val jDoubleArbitrary: Arbitrary[java.lang.Double] =
    mapArbitrary(implicitly[Arbitrary[Double]])(a => a)

  implicit val jFloatArbitrary: Arbitrary[java.lang.Float] =
    mapArbitrary(implicitly[Arbitrary[Float]])(a => a)

  implicit val jIntegerArbitrary: Arbitrary[java.lang.Integer] =
    mapArbitrary(implicitly[Arbitrary[Int]])(a => a)

  implicit val jLongArbitrary: Arbitrary[java.lang.Long] =
    mapArbitrary(implicitly[Arbitrary[Long]])(a => a)

  implicit val jShortArbitrary: Arbitrary[java.lang.Short] =
    mapArbitrary(implicitly[Arbitrary[Short]])(a => a)

  implicit val jBooleanArbitrary: Arbitrary[java.lang.Boolean] =
    mapArbitrary(implicitly[Arbitrary[Boolean]])(a => a)

  implicit val jCharacterArbitrary: Arbitrary[java.lang.Character] =
    mapArbitrary(implicitly[Arbitrary[Char]])(a => a)

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

  object EncodeDecodeInstances {
    implicit val testClassEncode: EncodeJson[TestClass] = EncodeJson.jencode22L((x: TestClass) => (x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h, x.i, x.j, x.k, x.l, x.m, x.n, x.o, x.p, x.q, x.r, x.s, x.t, x.u, x.v))("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")

    implicit val testClassDecode: DecodeJson[TestClass] = DecodeJson.jdecode22L(TestClass.apply)("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
  }

  object CodecInstances {
    implicit val testClassCodec: CodecJson[TestClass] = CodecJson.casecodec22(TestClass.apply, TestClass.unapply)("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
  }

  object derived {
    import TestTypes._

    val shapeDecodeJson: DecodeJson[Shape] = DecodeJson.derive[Circle] ||| DecodeJson.derive[Square]
    val circleEncodeJson: EncodeJson[Circle] = EncodeJson.derive[Circle]
    val squareEncodeJson: EncodeJson[Square] = EncodeJson.derive[Square]
    val shapeEncodeJson: EncodeJson[Shape] = EncodeJson{shape =>
      shape match {
        case c: Circle => circleEncodeJson(c)
        case s: Square => squareEncodeJson(s)
      }
    }
    implicit val shapeCodecJson: CodecJson[Shape] = CodecJson.derived(shapeEncodeJson, shapeDecodeJson)

    implicit def ProductCodecJson: CodecJson[Product] = CodecJson.derive[Product]
    implicit def OrderLineCodecJson: CodecJson[OrderLine] = CodecJson.derive[OrderLine]
    implicit def OrderCodecJson: CodecJson[Order] = CodecJson.derive[Order]
    implicit def PersonCodecJson: CodecJson[Person] = CodecJson.derive[Person]
    implicit def BackTicksCodecJson: CodecJson[BackTicks] = CodecJson.derive[BackTicks]

    def testDerivedPerson = encodedecode[Person]
    def testDerivedBackTicks = encodedecode[BackTicks]
    def testDerivedShape = encodedecode[Shape]
  }
}
