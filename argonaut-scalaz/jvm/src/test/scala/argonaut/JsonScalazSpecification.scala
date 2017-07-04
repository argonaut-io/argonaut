package argonaut

import argonaut.JsonScalaz._
import org.scalacheck._
import org.specs2._
import scalaz.Scalaz._
import scalaz._
import Data.ArbitraryJson

object JsonScalazSpecification extends Specification with ScalaCheck {

  private implicit def NonEmptyListStrGen: Gen[NonEmptyList[String]] = for {
    list <- Gen.nonEmptyListOf[String](Gen.alphaNumStr)
    res  <- list match {
      case x :: xs => Gen.const(NonEmptyList(x, xs: _*))
      case Nil     => Gen.fail[NonEmptyList[String]]
    }
  } yield res

  private implicit def NonEmptyListArb: Arbitrary[NonEmptyList[String]] =
    Arbitrary { NonEmptyListStrGen }


  private def FailedValidationGen[A]: Gen[NonEmptyList[String] => ValidationNel[String, A]] =
    Gen.const{ Validation.failure(_) }

  private implicit def FailedValidationArb[A]: Arbitrary[NonEmptyList[String] => ValidationNel[String, A]] =
    Arbitrary ( FailedValidationGen )

  private def alwaysFailsToDecodeProp: Prop =
    prop(
      (o: Json, failures: NonEmptyList[String], failed: NonEmptyList[String] => ValidationNel[String, Int]) => {
        val decoder: DecodeJson[Int] = asWithValidation[Int](_ => failed(failures))
        decoder.decode(o.hcursor) === DecodeResult.fail(failures.shows, CursorHistory.empty)
      }
    )

  private def alwaysDecodesSuccessfully: Prop = {
    prop(
      (o: Json) => {
        val decoder: DecodeJson[Boolean] = asWithValidation(_ => true.successNel[String])
        decoder.decode(o.hcursor) === DecodeResult.ok(true)
      }
    )
  }

  def is = s2"""
    JsonObjectScalaz
      asWithValidation with always failed validation      ${alwaysFailsToDecodeProp}
      asWithValidation with always successful validation  ${alwaysDecodesSuccessfully}
   """

}