package argonaut

import Argonaut._
import scalaz._, Scalaz._

/*
=============
JSON encoding
-------------
[
  "Fred",
  "40"
]

JSON decoding
-------------
Person(Fred,40)
=============

=============
JSON encoding
-------------
{
  "name" : "Fred",
  "age" : "40"
}

JSON decoding
-------------
Person(Fred,40)
=============
*/
object CodecDemo {
  case class Person(name: String, age: Int)

  object Person {
    val fred = Person("Fred", 40)

    trait Run {
      implicit val DecodePerson: DecodeJson[Person]
      implicit val EncodePerson: EncodeJson[Person]
      def run: (String, String) = {
        val enc = fred.jencode
        val encP = enc.spaces2
        val dec = enc.jdecode[Person]
        val decP =
          dec.result match {
            case Left(e) => e.shows
            case Right(p) => p.toString
          }
        (encP, decP)
      }
      def print {
        val (enc, dec) = run
        List(
          "============="
        , "JSON encoding"
        , "-------------"
        , enc
        , ""
        , "JSON decoding"
        , "-------------"
        , dec
        , "============="
        , ""
        ) foreach println
      }
    }

    object ArrayCodec extends Run {
      implicit val DecodePerson: DecodeJson[Person] =
        jdecode2(Person(_: String, _: Int)) setName "Person"

      implicit val EncodePerson: EncodeJson[Person] =
        jencode2((p: Person) => (p.name, p.age)) setName "Person"
    }

    object ObjectCodec extends Run {
      implicit val DecodePerson: DecodeJson[Person] =
        jdecode2L(Person(_: String, _: Int))("name", "age") setName "Person"

      implicit val EncodePerson: EncodeJson[Person] =
        jencode2L((p: Person) => (p.name, p.age))("name", "age") setName "Person"
    }

  }

  def main(args: Array[String]) {
    import Person._

    ArrayCodec.print
    ObjectCodec.print
  }
}
