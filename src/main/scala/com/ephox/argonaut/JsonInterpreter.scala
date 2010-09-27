package com.ephox.argonaut

import com.ephox.argonaut.ScalaToJava._
import com.ephox.argonaut._

// FIX 17924 27/09/2010 this guy is scala-friendly - make one that's java-friendly
object JsonInterpreter {

  // FIX 17924 27/09/2010 do we want to do something like this for java?
  // type Q[T] = Json => T

  def scalaList[T](sub: Json => T): (Json => List[T]) =
    (j: Json) => j.array.get.map(sub.apply(_))

  def javaList[T](sub: Json => T): (Json => java.util.List[T]) =
    (j: Json) => toJavaList(j.array.get.map(sub.apply(_)))

  def scalaTuples[T](sub: Json => T): (Json => List[(String, T)]) =
    (j: Json) => j.objectt.get.map { case (key, value) => (key, sub(value)) }

  val string = (_:Json).string.get
  val number = (_:Json).number.get

  val scalaListString: (Json) => List[String] = scalaList(string)
  val javaListString: (Json) => java.util.List[String] = javaList(string)

  val scalaListNumber = scalaList(number)
  val javaListNumber = javaList(number)

}
