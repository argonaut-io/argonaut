package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Gen
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object PrettyParamsJVMSpecification extends ArgonautSpec {
  def is = s2"""
  Parallelisation Safety
    vectorMemo        ${testMemo(PrettyParams.vectorMemo)}
  """

  def testMemo(memoFunction: (Int => String) => (Int => String)) = forAllNoShrink(Gen.choose(0, 100)) { count =>
    val f: Int => String = _.toString
    val memo = memoFunction(f)
    val notParallel = (1 to count).map(f).toList
    val isParallel = Await
      .result(
        Future.traverse(1 to count)(n => Future(memo(n))),
        5.seconds
      )
      .toList
    isParallel must beEqualTo(notParallel)
  }
}
