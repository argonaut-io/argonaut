package com.ephox.argonaut

import com.ephox.argonaut._
import com.ephox.argonaut.ScalaToJava._
import j.Interpret

object JsonInterpreter {

  // FIX 17924 27/09/2010 get these to return Option[T] or some Result[T]... or some other technique to make them fail without exploding

  implicit def mk[T](f: Json => T) = new Interpret[T] {
    override def apply(j: Json): T = f(j)
  }

  def scalaList[T](sub: Interpret[T]): Interpret[scala.List[T]] = (j: Json) =>
    j.array.get.map(sub.apply(_))

  def javaList[T](sub: Interpret[T]): Interpret[java.util.List[T]] = (j: Json) =>
    toJavaList(j.array.get.map(sub.apply(_)))

  val string: Interpret[String] = (j: Json) => j.string.get
  val number: Interpret[Double] = (j: Json) => j.number.get

  val scalaListString = scalaList(string)
  val javaListString = javaList(string)

  val scalaListNumber = scalaList(number)
  val javaListNumber = javaList(number)

}
