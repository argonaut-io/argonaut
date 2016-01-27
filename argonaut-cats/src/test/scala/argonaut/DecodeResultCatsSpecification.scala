package argonaut

import arbitrary._
import CursorHistoryCats._
import DecodeResultCats._
import cats.std.all._
import cats.laws.discipline.MonadTests
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.specs2.Specification
import org.typelevel.discipline.specs2.Discipline

/**
  * Created by luissanchez on 27/01/2016.
  */
class DecodeResultCatsSpecification extends Specification with Discipline {

  implicit val NumGen: Gen[Int] = Gen.posNum[Int]
  implicit val FunGen: Gen[Int => Int] = arbFunction1[Int, Int].arbitrary

  def is =
    br ^ br ^
    checkAll("DecodeResult[Int]", MonadTests[DecodeResult].monad[Int, Int, Int])
}
