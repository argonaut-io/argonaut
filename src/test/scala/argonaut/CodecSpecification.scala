package argonaut

import Data._
import JsonIdentity._
import scala.collection.immutable.{ SortedSet, SortedMap }
import scala.collection.mutable.ArrayBuffer
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary.{UnitArbitrary => _, _}
import org.scalacheck._, Arbitrary._, Prop._
import org.specs2._, org.specs2.specification._

object CodecSpecification extends Specification with ScalaCheck {
  implicit val shortEquality: Equal[Short] = new Equal[Short] {
    override def equal(a: Short, b: Short) = a == b
    override def equalIsNatural = true
  }

  implicit val jShortEquality: Equal[java.lang.Short] = new Equal[java.lang.Short] {
    override def equal(a: java.lang.Short, b: java.lang.Short) = a == b
    override def equalIsNatural = true
  }

  implicit def ArrayEquality[A: Equal]: Equal[Array[A]] =
    Equal[List[A]].contramap(_.toList)

  implicit def ArrayBufferEquality[A: Equal]: Equal[ArrayBuffer[A]] =
    Equal[List[A]].contramap(_.toList)

  implicit def SortedMapEquality[A: Equal]: Equal[SortedMap[String, A]] =
    Equal[Map[String, A]].contramap(m => m)

  def encodedecode[A: EncodeJson: DecodeJson: Arbitrary: Equal] =
    forAll(CodecJson.derived[A].codecLaw.encodedecode(_))

  implicit val SortedSetEquality = Equal[SortedSet[Int]]

  def is = s2"""
  Codec
    Unit encode/decode ${encodedecode[Unit]}
    Json encode/decode ${encodedecode[Json]}
    List[String] encode/decode ${encodedecode[List[String]]}
    List[Int] encode/decode ${encodedecode[List[Int]]}
    List[Long] encode/decode ${encodedecode[List[Long]]}
    List[Double] encode/decode ${encodedecode[List[Double]]}
    Vector[String] encode/decode ${encodedecode[Vector[String]]}
    Stream[String] encode/decode ${encodedecode[Stream[String]]}
    SortedSet[String] encode/decode ${encodedecode[SortedSet[String]]}
    SortedSet[Int] encode/decode ${encodedecode[SortedSet[Int]]}
    Array[String] encode/decode ${encodedecode[Array[String]]}
    ArrayBuffer[String] encode/decode ${encodedecode[ArrayBuffer[String]]}
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
    String \\/ Int encode/decode ${encodedecode[String \/ Int]}
    Map[String, Int] encode/decode ${encodedecode[Map[String, Int]]}
    SortedMap[String, Int] encode/decode ${encodedecode[SortedMap[String, Int]]}
    Set[String] encode/decode ${encodedecode[Set[String]]}
    ISet[Int] encode/decode ${encodedecode[ISet[Int]]}
    IList[Int] encode/decode ${encodedecode[IList[Int]]}
    NonEmptyList[Int] encode/decode ${encodedecode[NonEmptyList[Int]]}
    DList[Int] encode/decode ${encodedecode[DList[Int]]}
    EphemeralStream[Int] encode/decode ${encodedecode[EphemeralStream[Int]]}
    IMap[String, Int] encode/decode ${encodedecode[IMap[String, Int]]}
    Tuple2[String, Int] encode/decode ${encodedecode[Tuple2[String, Int]]}
    Tuple3[String, Int, Boolean] encode/decode ${encodedecode[Tuple3[String, Int, Boolean]]}
    Tuple4[String, Int, Boolean, Long] encode/decode ${encodedecode[Tuple4[String, Int, Boolean, Long]]}
    22 field class with codec ${import CodecInstances._; encodedecode[TestClass]}}
    22 field class with codec derived ${import EncodeDecodeInstances._; encodedecode[TestClass]}
    Witness Encode/Decode derived together ${ok}
    Witness Encode/Decode auto together ${ok}


  """

  implicit val jDoubleArbitrary: Arbitrary[java.lang.Double] =
    implicitly[Arbitrary[Double]].map(a => a)

  implicit val jFloatArbitrary: Arbitrary[java.lang.Float] =
    implicitly[Arbitrary[Float]].map(a => a)

  implicit val jIntegerArbitrary: Arbitrary[java.lang.Integer] =
    implicitly[Arbitrary[Int]].map(a => a)

  implicit val jLongArbitrary: Arbitrary[java.lang.Long] =
    implicitly[Arbitrary[Long]].map(a => a)

  implicit val jShortArbitrary: Arbitrary[java.lang.Short] =
    implicitly[Arbitrary[Short]].map(a => a)

  implicit val jBooleanArbitrary: Arbitrary[java.lang.Boolean] =
    implicitly[Arbitrary[Boolean]].map(a => a)

  implicit val jCharacterArbitrary: Arbitrary[java.lang.Character] =
    implicitly[Arbitrary[Char]].map(a => a)

  implicit val jDoubleEqual: Equal[java.lang.Double] = Equal.equalA
  implicit val jFloatEqual: Equal[java.lang.Float] = Equal.equalA
  implicit val jIntegerEqual: Equal[java.lang.Integer] = Equal.equalA
  implicit val jLongEqual: Equal[java.lang.Long] = Equal.equalA
  implicit val jBooleanEqual: Equal[java.lang.Boolean] = Equal.equalA
  implicit val jCharacterEqual: Equal[java.lang.Character] = Equal.equalA
  implicit def sortedSetEqual[A]: Equal[SortedSet[A]] = Equal.equalA

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

  object EncodeDecodeInstances {
    implicit val testClassEncode: EncodeJson[TestClass] = EncodeJson.jencode22L((x: TestClass) => (x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h, x.i, x.j, x.k, x.l, x.m, x.n, x.o, x.p, x.q, x.r, x.s, x.t, x.u, x.v))("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")

    implicit val testClassDecode: DecodeJson[TestClass] = DecodeJson.jdecode22L(TestClass.apply)("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
  }

  object CodecInstances {
    implicit val testClassEncode: CodecJson[TestClass] = CodecJson.casecodec22(TestClass.apply, TestClass.unapply)("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
  }

  object derived {
    case class Person(name: String, age: Int)

    implicit def PersonEncodeJson: EncodeJson[Person] =
      EncodeJson.derive[Person]

    implicit def PersonDecodeJson: DecodeJson[Person] =
      DecodeJson.derive[Person]

    EncodeJson.of[Person]
    DecodeJson.of[Person]

    CodecJson.derived[Person]
  }

  object auto {
    import shapeless._
    import EncodeJson.auto._
    import DecodeJson.auto._
    import StringWrap._
    import JsonIdentity._

    case class Person(name: String, age: Int)

    EncodeJson.of[Person]
    DecodeJson.of[Person]

    CodecJson.derived[Person]
  }
}
