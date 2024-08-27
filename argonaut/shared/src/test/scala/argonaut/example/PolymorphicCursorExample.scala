package argonaut.example

import argonaut.*
import Argonaut.*

object PolymorphicCursorExample extends ArgonautSpec {

  sealed trait Animal extends scala.Product with Serializable
  case class Dog(name: String) extends Animal
  case class Cat(age: Int) extends Animal

  implicit def AnimalsCodecJson: CodecJson[Animal] =
    CodecJson(
      (a: Animal) =>
        a match {
          case Dog(name) => ("type" := "dog") ->: ("name" := name) ->: jEmptyObject
          case Cat(age) => ("type" := "cat") ->: ("age" := age) ->: jEmptyObject
        },
      c =>
        for {
          klass <- (c --\ "type").as[String]
          result <- klass match {
            case "dog" => for { name <- (c --\ "name").as[String] } yield Dog(name)
            case "cat" => for { age <- (c --\ "age").as[Int] } yield Cat(age)
          }
        } yield result
    )

  def is = s2"""
    Serialize class hierarchy is possible ${val dog: Animal = Dog("kutyus")
    val dogJson: String = dog.asJson.nospaces
    dogJson.decodeOption[Animal] must_== Some(dog)}
    Serialize multiple object is also possible ${val animals = List(Dog("dogy"), Cat(12), Dog("digy"))
    val animalsJson: String = animals.asJson.nospaces
    animalsJson.decodeOption[List[Animal]] must_== Some(animals)}
    """
}
