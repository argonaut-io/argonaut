package com.ephox.argonaut

object JsonInterpreter2 {


  val string = (_:Json).string
  val number = (_:Json).number


//  def scalaArray[T](sub: Json => Option[T]) = (_:Json).number map sub


}
