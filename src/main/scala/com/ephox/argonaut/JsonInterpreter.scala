package com.ephox.argonaut

import com.ephox.argonaut._
import com.ephox.argonaut.ScalaToJava._
import j.Interpret

object JsonInterpreter {

  // FIX 17924 27/09/2010 get these to return Option[T]

  def scalaList[T](subbie: Interpret[T]): Interpret[scala.List[T]] = new Interpret[scala.List[T]] {
    override def apply(j: Json): scala.List[T] = j.array.get.map(subbie.apply(_))
  }

  def javaList[T](subbie: Interpret[T]): Interpret[java.util.List[T]] = new Interpret[java.util.List[T]] {
    override def apply(j: Json): java.util.List[T] = toJavaList(j.array.get.map(subbie.apply(_)))
  }

  def string: Interpret[String] = new Interpret[String] {
    override def apply(j: Json): String = j.string.get
  }

  def scalaListString = scalaList(string)
  def javaListString = javaList(string)

  
}
