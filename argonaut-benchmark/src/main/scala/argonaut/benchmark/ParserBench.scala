package argonaut
package benchmark

import Argonaut.*

object ParserBench {
  def parse(json: String, count: Int) = {
    val elapsed = Benchmark.time(() => json.parse, count) / 1000.0
    val bytes = json.getBytes("UTF-8").length
    val rate = (count / elapsed).toInt

    (bytes, rate)
  }

  def parseAndPrint(name: String, json: String, count: Int = 10000) = {
    parse(json, count) match {
      case (bytes, rate) =>
        printf("%s, %.5gKB @ %dmsg/s (%.2gMB/s)%n", name, bytes / 1024.0, rate, rate / ((1024 * 1024) / (bytes / 1.0)))
    }
  }
}
