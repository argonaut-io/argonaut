package argonaut

import arbitrary.*
import DecodeResultCats.*
import cats.laws.discipline.MonadTests
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.typelevel.discipline.specs2.Discipline

/**
  * Created by luissanchez on 27/01/2016.
  */
class DecodeResultCatsSpecification extends ArgonautSpec with Discipline {

  implicit val NumGen: Gen[Int] = Gen.posNum[Int]
  implicit val FunGen: Gen[Int => Int] = arbFunction1[Int, Int].arbitrary

  def is =
    br ^ br ^
      checkAll("DecodeResult[Int]", MonadTests[DecodeResult].monad[Int, Int, Int])
}
