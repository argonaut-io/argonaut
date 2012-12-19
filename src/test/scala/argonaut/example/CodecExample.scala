package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._
import org.specs2._
import org.specs2.specification._

object CodecExample extends Specification {
  case class Person(name: String, age: Int)

  val fred = Person("Fred", 40)

  def encodeDecode(json: String)(implicit encode: EncodeJson[Person], decode: DecodeJson[Person]) = {
    val person: Option[Person] = json.pparseDecode[Person]
    val encodedJson: Option[String] = person.map(_.jencode.nospaces)
    (person must be_==(fred.some)) and (encodedJson must be_==(json.some))
  }

  def is = "CodecExample" ^
    "Array codec" ! {
      implicit val DecodePerson: DecodeJson[Person] =
        jdecode2(Person(_: String, _: Int)) setName "Person"

      implicit val EncodePerson: EncodeJson[Person] =
        jencode2((p: Person) => (p.name, p.age)) setName "Person"
      
      encodeDecode("""["Fred","40"]""")
    } ^ 
    "Object codec" ! {
      implicit val DecodePerson: DecodeJson[Person] =
        jdecode2L(Person(_: String, _: Int))("name", "age") setName "Person"

      implicit val EncodePerson: EncodeJson[Person] =
        jencode2L((p: Person) => (p.name, p.age))("name", "age") setName "Person"

      encodeDecode("""{"name":"Fred","age":"40"}""")
    }
}
