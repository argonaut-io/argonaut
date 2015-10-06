package argonaut

import argonaut.Argonaut._
import org.specs2._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import Data._

object StringWrapSpecification extends Specification with ScalaCheck {
  case class Person(name: String, age: Int)

  implicit val DecodePerson: DecodeJson[Person] =
    jdecode2L(Person(_: String, _: Int))("name", "age").setName("Person")

  val validJSONTemplate: String = """{"name":"%s","age":"%s"}"""

  val invalidJSONTemplate: String = """{"name":"%s","ag":"%s"}"""

  def is = s2"""
  parse
    Optional encode ${
      (("optional" :?= (None: Option[String])) ->?: jEmptyObject) must_== jEmptyObject
    }
    Optional encode alias ${
      prop { (o: Option[Int]) =>
        (("optional" :?= o) ->?: jEmptyObject) must_== (("optional" :?= o) ->?: jEmptyObject)
      }
    }
    Optional encode precedence ${
      ("optional" :?= Some(1) filter (x => x > 0)) must_== Some("optional" := 1)
    }
    returns a success wrapped Json for valid JSON ${
      prop { (json: Json) =>
        json.nospaces.parse == Right(json)
      }
    }
    returns a failure for invalid JSON ${
      "{".parse == Left("JSON terminates unexpectedly.")
    }

    parseWith[X](Json => X, String => X): X
      returns the transformed Json for valid JSON ${
        prop { (json: Json) =>
          json.nospaces.parseWith(Some.apply, _ => None) == Some(json)
        }
      }
    returns a failure for invalid JSON ${
      "{".parseWith(_ => "Oops", identity) == "JSON terminates unexpectedly."
    }

    decode[X: DecodeJson]: String \\/ X
      returns the decoded Json for valid JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = validJSONTemplate.format(name, age)
          json.decode[Person] == Right(Person(name, age))
        }
      }
      returns a failure for invalid JSON ${
        "{".parseWith(_ => "Oops", identity) == "JSON terminates unexpectedly."
      }
      returns a failure for undecodable JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = invalidJSONTemplate.format(name, age)
          json.decode[Person].left.map(_.right.map(_._1)) must_== Left(Right("Person"))
        }
      }

    decodeWith[A, X: DecodeJson](X => A, String => A, (String, CursorHistory) => A): A
      returns the decoded and transformed Json for valid JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = validJSONTemplate.format(name, age)
          json.decodeWith[Option[Person], Person](Some.apply, _ => None, (_, _) => None) == Some(Person(name, age))
        }
      }  
      returns the result of the parseFailure function for invalid JSON ${
        "{".decodeWith[Option[Person], Person](_ => None, _ => Some(Person("Test", 5)), (_, _) => None) == Some(Person("Test", 5))
      }
      returns the result of the decodeFailure function for undecodable JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = invalidJSONTemplate.format(name, age)
          json.decodeWith[Option[Person], Person](_ => None, _ => None, (_, _) => Some(Person("Test", 5))) == Some(Person("Test", 5))
        }
      }

    decodeOr[A, X: DecodeJson](X => A, => A): A
      returns the decoded and transformed Json for valid JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = validJSONTemplate.format(name, age)
          json.decodeOr[Option[Person], Person](Some.apply, None) == Some(Person(name, age))
        }
      }
      returns the result of the default function for invalid JSON ${
        "{".decodeOr[Option[Person], Person](_ => None, Some(Person("Test", 5))) == Some(Person("Test", 5))
      }
      returns the result of the default function for undecodable JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = invalidJSONTemplate.format(name, age)
          json.decodeOr[Option[Person], Person](_ => None, Some(Person("Test", 5))) == Some(Person("Test", 5))
        }
      }

    parseOr[X](Json => X, => X): X
      returns the transformed Json for valid JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = validJSONTemplate.format(name, age)
          json.parseOr[Option[Json]](Some.apply, None) == Some(("age", jString(age.toString)) ->: ("name", jString(name)) ->: jEmptyObject)
        }
      }
      returns the result of the failure function for invalid JSON ${
        "{".parseOr[String](_ => "It works!", "Failure") == "Failure"
      }
      parseOption: Option[Json]
      returns Json wrapped in Some for valid JSON ${
        prop { (json: Json) =>
          json.nospaces.parseOption == Some(json)
        }
      }
      returns a failure for invalid JSON ${
        "{".parseOption == None
      }
      decodeOption[X: DecodeJson]: Option[X]
      returns the decoded value wrapped in a Some for valid JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = validJSONTemplate.format(name, age)
          json.decodeOption[Person] == Some(Person(name, age))
        }
      }
      returns a None for invalid JSON ${
        "{".decodeOption[Person] == None
      }
      returns a None for undecodable JSON ${
        forAllNoShrink(alphaStr, arbitrary[Int]) { (name: String, age: Int) =>
          val json = invalidJSONTemplate.format(name, age)
          json.decodeOption[Person] == None
        }
      }
  """
}
