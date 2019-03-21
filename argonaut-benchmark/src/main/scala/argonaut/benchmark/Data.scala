package argonaut
package benchmark

import scala.io.Source


object Data {
  def read(n: String) = {
    val cls = getClass()
    val cl = cls.getClassLoader()
    val is = cl.getResourceAsStream("json-data/" + n + ".json")
    Source.fromInputStream(is).mkString
  }

  lazy val example =
    read("example")

  lazy val integers =
    read("integers")

  lazy val jp10 =
    read("jp10")

  lazy val jp100 =
    read("jp100")

  lazy val jp50 =
    read("jp50")

  lazy val numbers =
    read("numbers")

  lazy val twitter1 =
    read("twitter1")

  lazy val twitter10 =
    read("twitter10")

  lazy val twitter100 =
    read("twitter100")

  lazy val twitter20 =
    read("twitter20")

  lazy val twitter50 =
    read("twitter50")

  lazy val apachebuilds =
    read("apache_builds")

  lazy val largestring =
    read("largestring")

  lazy val manystrings =
    read("manystrings")
}
