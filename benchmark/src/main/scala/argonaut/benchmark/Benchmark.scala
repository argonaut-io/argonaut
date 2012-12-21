package argonaut
package benchmark

object Benchmark {
  def time(run: () => Unit, count: Int = 1) = {
    val s = System.currentTimeMillis
    (1 to count).foreach(_ => run())
    val e = System.currentTimeMillis
    e - s
  }
}
