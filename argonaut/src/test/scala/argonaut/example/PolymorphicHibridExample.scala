package argonaut.example

import argonaut._, Argonaut._
import org.specs2._

object PolymorphicHibridExample extends Specification {

  sealed trait Animal
  case class Dog(name: String) extends Animal
  case class Cat(age: Int) extends Animal

  val CodecDog = casecodec1(Dog.apply, Dog.unapply)("name") // not implicit
  val CodecCat = casecodec1(Cat.apply, Cat.unapply)("age")  // not implicit

  implicit def AnimalsCodecJson : CodecJson[Animal] =
    CodecJson(
      (a: Animal) => a match {
        case dog@Dog(_) => Json("type" -> jString("dog"), "value" -> dog.asJson(CodecDog.Encoder))
        case cat@Cat(_) => Json("type" -> jString("cat"), "value" -> cat.asJson(CodecCat.Encoder))
      },
      c => for {
        klass <- (c --\ "type").as[String]
        result <- klass match {
          case "dog" => for {value <- (c --\ "value").jdecode(CodecDog.Decoder)} yield value
          case "cat" => for {value <- (c --\ "value").jdecode(CodecCat.Decoder)} yield value
        }
      } yield result
    )

  def is = s2"""
    Serialize class hierarchy is possible ${
      val dog = Dog("kutyus")
      val dogJson : String = dog.asJson.nospaces
      dogJson.decodeOption[Animal] must_== Some(dog)
    }
    Serialize multiple object is also possible ${
      val animals = List(Dog("dogy"), Cat(12), Dog("digy"))
      val animalsJson : String = animals.asJson.nospaces
      animalsJson.decodeOption[List[Animal]] must_== Some(animals)
    }
    """
}
