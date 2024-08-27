package argonaut

import org.scalacheck.*
import org.scalacheck.Gen.*

/**
  * Created by luissanchez on 27/01/2016.
  */
object arbitrary {

  // Some CursorOpElement
  val cursorOpElementCursorOpLeft: Gen[CursorOpElement] = Gen.const(CursorOpLeft)
  val cursorOpElementCursorOpRight: Gen[CursorOpElement] = Gen.const(CursorOpRight)
  val cursorOpElementCursorOpFirst: Gen[CursorOpElement] = Gen.const(CursorOpFirst)
  val cursorOpElementCursorOpLast: Gen[CursorOpElement] = Gen.const(CursorOpLast)
  val cursorOpElementCursorOpUp: Gen[CursorOpElement] = Gen.const(CursorOpUp)

  implicit val cursorOpElementGen: Gen[CursorOpElement] = Gen.oneOf(
    cursorOpElementCursorOpLeft,
    cursorOpElementCursorOpRight,
    cursorOpElementCursorOpFirst,
    cursorOpElementCursorOpLast,
    cursorOpElementCursorOpUp
  )
  // CursorOp
  def cursorOpEl(implicit F: Gen[CursorOpElement]): Gen[CursorOp] =
    zip(F, Gen.oneOf(true, false)).map((El.apply _).tupled)

  val cursorOpReattempt: Gen[CursorOp] = Gen.const(Reattempt)

  implicit val cursorOpsGen: Gen[CursorOp] = Gen.oneOf(cursorOpEl, cursorOpReattempt)

  implicit val cursorHistoryGen: Gen[CursorHistory] = Gen.listOf(cursorOpsGen).map(CursorHistory(_))

  implicit val cursorHistoryArb: Arbitrary[CursorHistory] = Arbitrary(cursorHistoryGen)

  implicit def decodeResultGen[A](implicit GenA: Gen[A]): Gen[DecodeResult[A]] =
    for {
      string <- alphaStr
      cursorHistory <- cursorHistoryGen
      a <- GenA
      generated <- Gen.oneOf(Left((string, cursorHistory)), Right(a))
    } yield DecodeResult(generated)

  implicit def decodeResultArb[A](implicit GenA: Gen[A]): Arbitrary[DecodeResult[A]] = Arbitrary(decodeResultGen(GenA))
}
