package argonaut.example

import argonaut.*
import Argonaut.*

object PolymorphicHibridExample extends ArgonautSpec {

  sealed trait Animal extends scala.Product with Serializable
  case class Dog(name: String) extends Animal
  case class Cat(age: Int) extends Animal

  val CodecDog = casecodec1(Dog.apply, (a: Dog) => Option(a.name))("name") // not implicit
  val CodecCat = casecodec1(Cat.apply, (a: Cat) => Option(a.age))("age") // not implicit

  implicit def AnimalsCodecJson: CodecJson[Animal] =
    CodecJson(
      {
        case dog @ Dog(_) => Json("type" -> jString("dog"), "value" -> dog.asJson(using CodecDog.Encoder))
        case cat @ Cat(_) => Json("type" -> jString("cat"), "value" -> cat.asJson(using CodecCat.Encoder))
      },
      c =>
        for {
          klass <- (c --\ "type").as[String]
          result <- klass match {
            case "dog" => for { value <- (c --\ "value").jdecode(using CodecDog.Decoder) } yield value
            case "cat" => for { value <- (c --\ "value").jdecode(using CodecCat.Decoder) } yield value
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
