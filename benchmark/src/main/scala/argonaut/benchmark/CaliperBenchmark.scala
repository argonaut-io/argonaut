package argonaut.benchmark

import com.google.caliper._
import argonaut._
import Argonaut._
import net.liftweb.json.{JsonParser => LiftJsonParser}
import scalaz._
import Scalaz._
import com.fasterxml.jackson.core.{TreeNode, JsonFactory => JacksonJsonFactory}

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

object CaliperJacksonBenchmarkRunner {
  def main(args: Array[String]) {
    Runner.main(classOf[CaliperLiftBenchmark], args)
  }
}

object CaliperScalaUtilJSONBenchmarkRunner {
  def main(args: Array[String]) {
    Runner.main(classOf[CaliperScalaUtilJSONBenchmark], args)
  }
}


case class CaliperArgonautBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps)(json.parse)
}

case class CaliperLiftBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps)(LiftJsonParser.parse(json))
  override def timenumbers(reps: Int) = repeat(reps){Thread.sleep(1); LiftJsonParser.parse("""["lift-json sucks and breaks on this benchmark."]""")}
}

object CaliperJacksonBenchmark {
  val jsonFactory = new JacksonJsonFactory()
}

case class CaliperJacksonBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps){
    val parser = CaliperJacksonBenchmark.jsonFactory.createParser(json)
    if (parser.readValueAsTree[TreeNode]().asToken() == null) 0 else 1
  }
}

case class CaliperScalaUtilJSONBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps){
    if (scala.util.parsing.json.JSON.parseFull(json).isEmpty) 0 else 1
  }
}

object ArgonautSimpleBench {
  def main(args: Array[String]) {
    Thread.sleep(10000)
    (0 to 1000).foldLeft(0){(left, right) => Data.twitter100.parse; Data.twitter10.length + left}
  }
}

trait CaliperBenchmark extends SimpleScalaBenchmark {
  def repeatParse(json: String, reps: Int): Unit

  val stringList = (0 to 200).toList.map(_ => "aaaaaaaaaaaaaaaa")

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
  //def timecordappend(reps: Int) = repeat(reps)(stringList.foldLeft(Cord.empty)(_ :+ _).toString)
  //def timestringbuilderappend(reps: Int) = repeat(reps)(stringList.foldLeft(new java.lang.StringBuilder)(_ append _).toString)
  //def timelistappendreverse(reps: Int) = repeat(reps)(stringList.foldLeft(List.empty[String])((left, right) => right :: left).reverse)
  //def timevectorappend(reps: Int) = repeat(reps)(stringList.foldLeft(Vector.empty[String])(_ :+ _).toList)
  //def timedlistappend(reps: Int) = repeat(reps)(stringList.foldLeft(DList[String]())(_ :+ _).toList)
  //def timelistbuilderappend(reps: Int) = repeat(reps)(stringList.foldLeft(List.newBuilder[String])(_ += _).result)
}
