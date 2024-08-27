package argonaut

import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

/**
  * Created by luissanchez on 27/01/2016.
  */
class EncodeJsonCatsSpecification extends ArgonautSpec {

  def is = s2"""
  contravariants laws must hold for EncodeJson[Int]
    contravariant identity    ${contravariantIdentity}
    contravariant composition ${contravariantcomposition}
  """.stripMargin

  def contravariantIdentity = forAll(Gen.posNum[Int]) { a =>
    val left = EncodeJson.IntEncodeJson.contramap(identity[Int])
    val right = EncodeJson.IntEncodeJson

    left.encode(a) must_== right.encode(a)
  }

  def contravariantcomposition =
    forAll(Gen.posNum[Int], arbFunction1[Int, Int].arbitrary, arbFunction1[Int, Int].arbitrary) { (a, f, g) =>
      val left = EncodeJson.IntEncodeJson.contramap(f).contramap(g)
      val right = EncodeJson.IntEncodeJson.contramap(f compose g)

      left.encode(a) must_== right.encode(a)
    }
}
