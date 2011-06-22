package com.ephox
package argonaut

import JsonInterpreter._
import StringWrap._

object Experiment {
  def main(args: Array[String]) {


    val y = "[\"a\", \"b\", \"c\"]".parseTo(javaListString)
    println(y.successful)
    println(y.get)

    try {
      val y2 = "[1, \"b\", \"c\"]".parseTo(scalaListString)
      println(y2.successful)
      println(y2.get)
    } catch {
      case x: Exception => x.printStackTrace()
    }

    val q = "[".parseTo(scalaList(number))
  }
}
