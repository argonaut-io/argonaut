package argonaut
package benchmark

object Benchmark {
  def time(run: () => Unit, count: Int = 1, preRunCount: Int = 500) = {
    for (i <- 1 to preRunCount) run()
    val s = System.currentTimeMillis
    for (i <- 1 to count) run()
    val e = System.currentTimeMillis
    e - s
  }
}
