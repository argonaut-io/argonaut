package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import org.specs2._
import org.specs2.specification._

object CodecExample extends Specification {
  case class Person(name: String, age: Int)

  val fred = Person("Fred", 40)

  def encodeDecode(json: String)(implicit encode: EncodeJson[Person], decode: DecodeJson[Person]) = {
    val person: Option[Person] = json.decodeOption[Person]
    val encodedJson: Option[String] = person.map(_.jencode.nospaces)
    (person must be_==(fred.some)) and (encodedJson must be_==(json.some))
  }

  def is = s2"""
  Array codec ${
    implicit val DecodePerson: DecodeJson[Person] =
      jdecode2(Person(_: String, _: Int))

    implicit val EncodePerson: EncodeJson[Person] =
      jencode2((p: Person) => (p.name, p.age))

    encodeDecode("""["Fred",40]""")
  }
  Object codec ${
    implicit val DecodePerson: DecodeJson[Person] =
      jdecode2L(Person(_: String, _: Int))("name", "age")

    implicit val EncodePerson: EncodeJson[Person] =
      jencode2L((p: Person) => (p.name, p.age))("name", "age")

    encodeDecode("""{"name":"Fred","age":40}""")
  }
  Object codec with filtering ${
    implicit val DecodePersonOver30: DecodeJson[Person] =
      DecodeJson(c =>
        for {
          name <- (c --\ "name").as[String]
          age <- (c --\ "age").as[Int] if age > 30
        } yield Person(name, age)
      )

    implicit val EncodePerson: EncodeJson[Person] =
      jencode2L((p: Person) => (p.name, p.age))("name", "age")

    encodeDecode("""{"name":"Fred","age":40}""")
    
    """{"name":"Fred","age":20}""".decodeOption[Person] must beNone
  }
  """
}
