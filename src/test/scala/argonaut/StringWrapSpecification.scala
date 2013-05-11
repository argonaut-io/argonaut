package argonaut

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import org.specs2._, org.specs2.specification._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import Data._

object StringWrapSpecification extends Specification with ScalaCheck {
  case class Person(name: String, age: Int)

  implicit val DecodePerson: DecodeJson[Person] =
    jdecode2L(Person(_: String, _: Int))("name", "age").setName("Person")

  implicit val EqualPerson: Equal[Person] = Equal.equalA[Person]

  val validJSONTemplate: String = """{"name":"%s","age":"%s"}"""

  val invalidJSONTemplate: String = """{"name":"%s","ag":"%s"}"""

  def is =
    "parse" ^
      "returns a success wrapped Json for valid JSON" ! prop{(json: Json) =>
        json.nospaces.parse === json.right[String]
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parse === "JSON terminates unexpectedly".left
      } ^ end ^
    "parseWith[X](Json => X, String => X): X" ^
      "returns the transformed Json for valid JSON" ! prop{(json: Json) =>
	json.nospaces.parseWith(_.some, _ => None) === json.some
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parseWith(_ => "Oops", identity) === "JSON terminates unexpectedly"
      } ^ end ^
    "decode[X: DecodeJson]: String \\/ X" ^
      "returns the decoded Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.decode[Person] === Person(name, age).right
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parseWith(_ => "Oops", identity) === "JSON terminates unexpectedly"
      } ^
      "returns a failure for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = invalidJSONTemplate.format(name, age)
	json.decode[Person].swapped(_.map(_.map(_._1))) must_==  "Person".right.left
      } ^ end ^
    "decodeWith[A, X: DecodeJson](X => A, String => A, (String, CursorHistory) => A): A" ^
      "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = validJSONTemplate.format(name, age)
        json.decodeWith[Option[Person], Person](_.some, _ => None, (_, _) => None) === Person(name, age).some
      } ^
      "returns the result of the parseFailure function for invalid JSON" ! {
	"{".decodeWith[Option[Person], Person](_ => None, _ => Person("Test", 5).some, (_, _) => None) === Person("Test", 5).some
      } ^
      "returns the result of the decodeFailure function for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = invalidJSONTemplate.format(name, age)
        json.decodeWith[Option[Person], Person](_ => None, _ => None, (_, _) => Person("Test", 5).some) === Person("Test", 5).some
      } ^ end ^
    "decodeOr[A, X: DecodeJson](X => A, => A): A" ^
      "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = validJSONTemplate.format(name, age)
        json.decodeOr[Option[Person], Person](_.some, None) === Person(name, age).some
      } ^
      "returns the result of the default function for invalid JSON" ! {
	"{".decodeOr[Option[Person], Person](_ => None, Person("Test", 5).some) === Person("Test", 5).some
      } ^
      "returns the result of the default function for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = invalidJSONTemplate.format(name, age)
        json.decodeOr[Option[Person], Person](_ => None, Person("Test", 5).some) === Person("Test", 5).some
      } ^ end ^
    "parseOr[X](Json => X, => X): X" ^
      "returns the transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = validJSONTemplate.format(name, age)
        json.parseOr[Option[Json]](_.some, None) === Some(("age", jString(age.toString)) ->: ("name", jString(name)) ->: jEmptyObject)
      } ^
      "returns the result of the failure function for invalid JSON" ! {
	"{".parseOr[String](_ => "It works!", "Failure") === "Failure"
      } ^ end ^
    "parseOption: Option[Json]" ^
      "returns Json wrapped in Some for valid JSON" ! prop{(json: Json) =>
	json.nospaces.parseOption === json.some
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parseOption === None
      } ^ end ^
    "decodeOption[X: DecodeJson]: Option[X]" ^
      "returns the decoded value wrapped in a Some for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = validJSONTemplate.format(name, age)
        json.decodeOption[Person] === Person(name, age).some
      } ^
      "returns a None for invalid JSON" ! {
	"{".decodeOption[Person] === None
      } ^
      "returns a None for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
        val json = invalidJSONTemplate.format(name, age)
        json.decodeOption[Person] === None
      } ^ end
}
