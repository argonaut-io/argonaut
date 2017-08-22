package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Gen
import CompatParColls.Converters._

object PrettyParamsJVMSpecification extends ArgonautSpec {
  def is = s2"""
  Parallelisation Safety
    vectorMemo        ${testMemo(PrettyParams.vectorMemo)}
  """

  def testMemo(memoFunction: (Int => String) => (Int => String)) = forAllNoShrink(Gen.choose(0, 100)){count =>
    val f: Int => String = _.toString
    val memo = memoFunction(f)
    val notParallel = (1 to count).map(f).toList
    val isParallel = (1 to count).par.map(memo).toList
    isParallel must beEqualTo(notParallel)
  }
}
