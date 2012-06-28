package com.ephox
package argonaut

import Argonaut._

object CodecDemo {
  case class Person(name: String, age: Int, spouse: Option[Person], children: List[Person])

  object Person {
    implicit val DecodePerson: DecodeJson[Person] =
      DecodeJson(j => j.array match {
        case Some(n::a::s::c::Nil) =>
          for {
            nn <- implicitly[DecodeJson[String]].apply(n)
            aa <- implicitly[DecodeJson[Int]].apply(a)
            ss <- implicitly[DecodeJson[Option[Person]]].apply(s)
            cc <- implicitly[DecodeJson[List[Person]]].apply(c)
          } yield Person(nn, aa, ss, cc)
        case _ => decodeError(j, "Person")
      })

    implicit val EncodePerson: EncodeJson[Person] =
      EncodeJson({
        case Person(n, a, s, c) => jArray(List(
          implicitly[EncodeJson[String]].apply(n)
        , implicitly[EncodeJson[Int]].apply(a)
        , implicitly[EncodeJson[Option[Person]]].apply(s)
        , implicitly[EncodeJson[List[Person]]].apply(c)
        ))
      }, "Person")
  }

  def main(args: Array[String]) {
    val children = List(Person("Bob", 10, None, Nil), Person("Jill", 12, None, Nil))
    val fred = Person("Fred", 40, Some(Person("Mary", 41, None, children)), Person("Tom", 15, None, Nil) :: children)
    val encode = implicitly[EncodeJson[Person]].apply(fred)
    println(encode.spaces2)
    val decode = implicitly[DecodeJson[Person]].apply(encode)
    println(decode.run match {
      case Left(e) => e
      case Right(p) => p.toString
    })
  }
}
