package argonaut.benchmark

import com.google.caliper._
import argonaut._
import Argonaut._
import net.liftweb.json.{JsonParser => LiftJsonParser}

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
  def main(args: Array[String]) {
    Runner.main(classOf[CaliperArgonautBenchmark], args)
  }
}

object CaliperLiftBenchmarkRunner {
  def main(args: Array[String]) {
    Runner.main(classOf[CaliperLiftBenchmark], args)
  }
}


case class CaliperArgonautBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps)(json.parse)
}

case class CaliperLiftBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps)(LiftJsonParser.parse(json))
  override def timenumbers(reps: Int) = repeat(reps)(LiftJsonParser.parse("""["lift-json sucks and breaks on this benchmark."]"""))
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
}
