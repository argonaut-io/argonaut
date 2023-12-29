package argonaut

import org.scalacheck.Arbitrary
import org.specs2.scalacheck.ScalaCheckFunction1

object CodecSpecificationScala3 extends ArgonautSpec {
  def encodedecode[A: EncodeJson: DecodeJson: Arbitrary]: ScalaCheckFunction1[A, Boolean] = {
    val aCodec = CodecJson.derived[A]
    prop[A, Boolean] { a =>
      CodecJson.codecLaw(aCodec)(a)
    }
  }

  def is = s2"""
  Codec
    CodecJson[MyList[A]] derived ${testMyList}
  """

  private def testMyList = {
    encodedecode[MyList[String]]
    encodedecode[MyList[Int]]
  }
}

case class MyList[A](head: A, tail: Option[MyList[A]])

object MyList {
  implicit def decodeJson[A: DecodeJson]: DecodeJson[MyList[A]] =
    DecodeJson.derive[MyList[A]]

  implicit def encodeJson[A: EncodeJson]: EncodeJson[MyList[A]] =
    EncodeJson.derive[MyList[A]]

  @annotation.tailrec
  private def fromList[A](list: List[A], acc: MyList[A]): MyList[A] = {
    list match {
      case x :: xs =>
        fromList(xs, MyList(x, Some(acc)))
      case Nil =>
        acc
    }
  }

  implicit def arbitrary[A: Arbitrary]: Arbitrary[MyList[A]] = Arbitrary(
    implicitly[Arbitrary[(A, List[A])]].arbitrary.map { case (x, xs) =>
      fromList(xs, MyList(x, None))
    }
  )
}
