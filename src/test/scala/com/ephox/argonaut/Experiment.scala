package com.ephox.argonaut

import com.ephox.argonaut.JsonInterpreter._
import JsonParser._

object Experiment {
  def main(args: Array[String]) {


    val y = parseTo(javaListString, "[\"a\", \"b\", \"c\"]");
    println(y.successful)
    println(y.get)

    try {
      val y2 = parseTo(scalaListString, "[1, \"b\", \"c\"]");
      println(y2.successful)
      println(y2.get)
    } catch {
      case x: Exception => x.printStackTrace()
    }

    val q = parseTo(scalaList(number), "[")
  }
}
