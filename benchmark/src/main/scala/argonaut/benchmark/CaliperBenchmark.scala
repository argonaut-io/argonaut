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

object CaliperArgonautExtraBenchmarkRunner {
  def main(args: Array[String]) {
    Runner.main(classOf[CaliperArgonautExtraBenchmark], args)
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

object CaliperJawnBenchmarkRunner {
  def main(args: Array[String]) {
    Runner.main(classOf[CaliperJawnBenchmark], args)
  }
}


case class CaliperArgonautBenchmark() extends CaliperBenchmark {
  override def repeatParse(json: String, reps: Int): Unit = repeat(reps)(json.parse)
}

case class CaliperArgonautExtraBenchmark() extends SimpleScalaBenchmark {
  val jsonToPrint = Data.apachebuilds.parseOption.get
  val smallJsonToPrint = jSingleObject("array", jArray(List(jNumber(5), jTrue, jFalse)))
  def timesmallnospaces(reps: Int) = repeat(reps){
    smallJsonToPrint.nospaces.length
  }
  def timesmallspaces4(reps: Int) = repeat(reps){
    smallJsonToPrint.spaces4.length
  }
  def timenospaces(reps: Int) = repeat(reps){
    jsonToPrint.nospaces.length
  }
  def timespaces4(reps: Int) = repeat(reps){
    jsonToPrint.spaces4.length
  }
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

case class CaliperJawnBenchmark() extends CaliperBenchmark {
  import jawn._

  // via @d6 with https://github.com/non/jawn/blob/topic/facade/benchmark/src/main/scala/jawn/AdHocBenchmarks.scala

  val ArgonautFacade = new Facade[argonaut.Json] {
    def singleContext() = new FContext[argonaut.Json] {
      var value: argonaut.Json = null
      def add(s: String) { value = jstring(s) }
      def add(v: argonaut.Json) { value = v }
      def finish: argonaut.Json = value
      def isObj: Boolean = false
    }

    def arrayContext() = new FContext[argonaut.Json] {
      val vs = scala.collection.mutable.ArrayBuffer.empty[argonaut.Json]
      def add(s: String) { vs.append(jstring(s)) }
      def add(v: argonaut.Json) { vs.append(v) }
      def finish: argonaut.Json = argonaut.Json.jArray(vs.toList)
      def isObj: Boolean = false
    }

    def objectContext() = new FContext[argonaut.Json] {
      var key: String = null
      var vs = scalaz.InsertionMap.empty[String, argonaut.Json]
      //val vs = mutable.Map.empty[String, argonaut.Json]
      def add(s: String): Unit = if (key == null) {
        key = s
      } else {
        //vs(key) = jstring(s)
        vs = vs ^+^ (key, jstring(s))
        key = null
      }

      def add(v: argonaut.Json): Unit = {
        //vs(key) = v
        vs = vs ^+^ (key, v)
        key = null
      }

      def finish = argonaut.Json.jObjectMap(vs)
      def isObj = true
    }

    def jnull() = argonaut.Json.jNull
    def jfalse() = argonaut.Json.jFalse
    def jtrue() = argonaut.Json.jTrue
    def jnum(s: String) = argonaut.Json.jNumber(s.toDouble)
    def jstring(s: String) = argonaut.Json.jString(s)
  }

  override def repeatParse(json: String, reps: Int): Unit = repeat(reps){
    implicit val facade = ArgonautFacade
    jawn.GenericParser.parseFromString[argonaut.Json](json).right.get
  }
}

object ArgonautSimpleBench {
  def main(args: Array[String]) {
    Thread.sleep(10000)
    (0 to 3000).foldLeft(0l){(left, right) => left + Data.example.parseOption.get.spaces4.length + right}
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
}
