package argonaut
package benchmark

object Main {
  def main(args: Array[String]) = {
    ParserBench.parseAndPrint("example", Data.example)
    ParserBench.parseAndPrint("integers", Data.integers)
    ParserBench.parseAndPrint("jp10", Data.jp10)
    ParserBench.parseAndPrint("jp100", Data.jp100)
    ParserBench.parseAndPrint("jp50", Data.jp50)
    ParserBench.parseAndPrint("numbers", Data.numbers)
    ParserBench.parseAndPrint("twitter1", Data.twitter1)
    ParserBench.parseAndPrint("twitter10", Data.twitter10)
    ParserBench.parseAndPrint("twitter100", Data.twitter100)
    ParserBench.parseAndPrint("twitter20", Data.twitter20)
    ParserBench.parseAndPrint("twitter50", Data.twitter50)
  }
}
