package argonaut.benchmark

import com.google.caliper.*
import argonaut.*
import Argonaut.*
import com.fasterxml.jackson.core.TreeNode
import com.fasterxml.jackson.core.JsonFactory as JacksonJsonFactory

// Stolen conveniently from
// https://github.com/sirthias/scala-benchmarking-template/blob/master/src/main/scala/org/example/SimpleScalaBenchmark.scala.
trait SimpleScalaBenchmark extends SimpleBenchmark {

  // helper method to keep the actual benchmarking methods a bit cleaner
  // your code snippet should always return a value that cannot be "optimized away"
  def repeat[@specialized A](reps: Int)(snippet: => A) = {
    val zero = 0.asInstanceOf[A] // looks weird but does what it should: init w/ default value in a fully generic way
    var i = 0
    var result = zero
    while (i < reps) {
      val res = snippet
      if (res != zero) result = res // make result depend on the benchmarking snippet result
      i = i + 1
    }
    result
  }
}

object CaliperArgonautBenchmarkRunner {
  def main(args: Array[String]): Unit = {
    Runner.main(classOf[CaliperArgonautBenchmark], args)
  }
}

object CaliperJacksonBenchmarkRunner {
  def main(args: Array[String]): Unit = {
    Runner.main(classOf[CaliperJacksonBenchmark], args)
  }
}

case class CaliperArgonautBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps)(json.parse)

  val largeString = Data.largestring.parseOption.get
  val manyStrings = Data.manystrings.parseOption.get

  def timelargestringnospaces(reps: Int) = repeat(reps) {
    largeString.nospaces.length
  }

  def timemanystringsnospaces(reps: Int) = repeat(reps) {
    manyStrings.nospaces.length
  }

  val jsonToPrint = Data.apachebuilds.parseOption.get
  val smallJsonToPrint = jSingleObject("array", jArray(jNumber(5) :: List(jTrue, jFalse)))
  def timesmallnospaces(reps: Int) = repeat(reps) {
    smallJsonToPrint.nospaces.length
  }
  def timesmallspaces4(reps: Int) = repeat(reps) {
    smallJsonToPrint.spaces4.length
  }
  def timenospaces(reps: Int) = repeat(reps) {
    jsonToPrint.nospaces.length
  }
  def timespaces4(reps: Int) = repeat(reps) {
    jsonToPrint.spaces4.length
  }
}

object CaliperJacksonBenchmark {
  val jsonFactory = new JacksonJsonFactory()
}

case class CaliperJacksonBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps) {
    val parser = CaliperJacksonBenchmark.jsonFactory.createParser(json)
    if (parser.readValueAsTree[TreeNode]().asToken() == null) 0 else 1
  }
}

object ArgonautSimpleBench {
  def main(args: Array[String]): Unit = {
    Thread.sleep(10000)
    (0 to 3000).foldLeft(0L) { (left, right) => left + Data.example.parseOption.get.spaces4.length + right }
  }
}

trait CaliperBenchmark extends SimpleScalaBenchmark {
  def repeatParse(json: String, reps: Int): Unit

  def timeexample(reps: Int) = repeatParse(Data.example, reps)
  def timeintegers(reps: Int) = repeatParse(Data.integers, reps)
  def timejp10(reps: Int) = repeatParse(Data.jp10, reps)
  def timejp100(reps: Int) = repeatParse(Data.jp100, reps)
  def timejp50(reps: Int) = repeatParse(Data.jp50, reps)
  def timenumbers(reps: Int) = repeatParse(Data.numbers, reps)
  def timetwitter1(reps: Int) = repeatParse(Data.twitter1, reps)
  def timetwitter10(reps: Int) = repeatParse(Data.twitter10, reps)
  def timetwitter100(reps: Int) = repeatParse(Data.twitter100, reps)
  def timetwitter20(reps: Int) = repeatParse(Data.twitter20, reps)
  def timetwitter50(reps: Int) = repeatParse(Data.twitter50, reps)
  def timeapachebuilds(reps: Int) = repeatParse(Data.apachebuilds, reps)
  def timelargestring(reps: Int) = repeatParse(Data.largestring, reps)
  def timemanystrings(reps: Int) = repeatParse(Data.manystrings, reps)
}
