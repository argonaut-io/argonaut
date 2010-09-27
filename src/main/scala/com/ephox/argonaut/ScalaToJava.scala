package com.ephox.argonaut

object ScalaToJava {
  def toJavaList[T](list: scala.List[T]): java.util.List[T] = {
    val r = new java.util.LinkedList[T]
    list.foreach(r.add(_))
    r
  }
}
