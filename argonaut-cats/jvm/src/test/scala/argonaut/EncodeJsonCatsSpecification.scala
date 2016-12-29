package argonaut

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by luissanchez on 27/01/2016.
  */
class EncodeJsonCatsSpecification extends Specification with ScalaCheck {

  def is =s2"""
  contravariants laws must hold for EncodeJson[Int]
    contravariant identity    ${contravariantIdentity}
    contravariant composition ${contravariantcomposition}
  """.stripMargin

  def contravariantIdentity = forAll(Gen.posNum[Int]) { a =>
      val left = EncodeJson.IntEncodeJson.contramap(identity[Int])
      val right = EncodeJson.IntEncodeJson

      left.encode(a) must_== right.encode(a)
  }

  def contravariantcomposition = forAll(Gen.posNum[Int], arbFunction1[Int, Int].arbitrary, arbFunction1[Int, Int].arbitrary) {
    (a, f, g) =>
    val left = EncodeJson.IntEncodeJson.contramap(f).contramap(g)
    val right = EncodeJson.IntEncodeJson.contramap(f compose g)

    left.encode(a) must_== right.encode(a)
  }
}
