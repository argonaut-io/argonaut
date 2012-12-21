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
    "parse()" ^
      "returns a success wrapped Json for valid JSON" ! prop{(json: Json) =>
	json.nospaces.parse() === json.successNel[String]
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parse() === "Expected string bounds but found: ".failNel
      } ^ end ^
    "parse[X](Json => X, NonEmptyList[String] => X): X" ^
      "returns the transformed Json for valid JSON" ! prop{(json: Json) =>
	json.nospaces.parse(_.some, _ => None) === json.some
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parse(_ => NonEmptyList("Oops"), identity) === NonEmptyList("Expected string bounds but found: ")
      } ^ end ^
    "parseDecode[X: DecodeJson](): ValidationNEL[String, X]" ^
      "returns the decoded Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.parseDecode[Person]() === Person(name, age).successNel
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parse(_ => NonEmptyList("Oops"), identity) === NonEmptyList("Expected string bounds but found: ")
      } ^
      "returns a failure for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = invalidJSONTemplate.format(name, age)
	json.parseDecode[Person]() === """Failure decoding JSON: [*.--\(age)]""".failNel
      } ^ end ^
    "parseDecodeWith[A, X: DecodeJson](X => A, NonEmptyList[String] => A, (String, CursorHistory) => A): A" ^
      "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.parseDecodeWith[Option[Person], Person](_.some, _ => None, (_, _) => None) === Person(name, age).some
      } ^
      "returns the result of the parseFailure function for invalid JSON" ! {
	"{".parseDecodeWith[Option[Person], Person](_ => None, _ => Person("Test", 5).some, (_, _) => None) === Person("Test", 5).some
      } ^
      "returns the result of the decodeFailure function for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = invalidJSONTemplate.format(name, age)
	json.parseDecodeWith[Option[Person], Person](_ => None, _ => None, (_, _) => Person("Test", 5).some) === Person("Test", 5).some
      } ^ end ^
    "parseDecodeOr[A, X: DecodeJson](X => A, => A): A" ^
      "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.parseDecodeOr[Option[Person], Person](_.some, None) === Person(name, age).some
      } ^
      "returns the result of the default function for invalid JSON" ! {
	"{".parseDecodeOr[Option[Person], Person](_ => None, Person("Test", 5).some) === Person("Test", 5).some
      } ^
      "returns the result of the default function for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = invalidJSONTemplate.format(name, age)
	json.parseDecodeOr[Option[Person], Person](_ => None, Person("Test", 5).some) === Person("Test", 5).some
      } ^ end ^
    "parseDecodeLiftParseFailures[X: DecodeJson](NonEmptyList[String] => String): DecodeResult[X]" ^
      "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.parseDecodeLiftParseFailures[Person](_ => "Error").result === Person(name, age).right
      } ^
      "returns the result of the parseFailure function for invalid JSON" ! {
	"{".parseDecodeLiftParseFailures[Person](_ => "Error").result === ("Error", CursorHistory.build(Nil)).left
      } ^
      "returns the decode failure for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = invalidJSONTemplate.format(name, age)
	json.parseDecodeLiftParseFailures[Person](_ => "Error").result === ("Person", CursorHistory.build(List(El(CursorOpDownField("age"), false)))).left
      } ^ end ^
    "parseIgnoreError[X](Json => X, => X): X" ^
      "returns the transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.parseIgnoreError[Option[Json]](_.some, None) === Some(("age", jString(age.toString)) ->: ("name", jString(name)) ->: jEmptyObject)
      } ^
      "returns the result of the failure function for invalid JSON" ! {
	"{".parseIgnoreError[String](_ => "It works!", "Failure") === "Failure"
      } ^ end ^
    "parseTo[X](Json => X): ValidationNEL[String, X]" ^
      "returns a success wrapped transformed value for valid JSON" ! prop{(json: Json) =>
	json.nospaces.parseTo[Option[Json]](_.some) === json.some.successNel[String]
      } ^
      "returns a failure for invalid JSON" ! {
	"{".parseTo[Option[Json]](_.some) === "Expected string bounds but found: ".failNel
      } ^ end ^
    "parseToOrDie[X](Json => X): X" ^
      "returns a transformed value for valid JSON" ! prop{(json: Json) =>
	json.nospaces.parseToOrDie[Option[Json]](_.some) === json.some
      } ^
      "throws an exception for invalid JSON" ! {
	"{".parseToOrDie[Option[Json]](_.some) must throwA[RuntimeException]
      } ^ end ^
    "pparse(): Option[Json]" ^
      "returns Json wrapped in Some for valid JSON" ! prop{(json: Json) =>
	json.nospaces.pparse === json.some
      } ^
      "returns a failure for invalid JSON" ! {
	"{".pparse === None
      } ^ end ^
    "pparseDecode[X: DecodeJson](): Option[X]" ^
      "returns the decoded value wrapped in a Some for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = validJSONTemplate.format(name, age)
	json.pparseDecode[Person] === Person(name, age).some
      } ^
      "returns a None for invalid JSON" ! {
	"{".pparseDecode[Person] === None
      } ^
      "returns a None for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
	val json = invalidJSONTemplate.format(name, age)
	json.pparseDecode[Person] === None
      } ^ end
}
