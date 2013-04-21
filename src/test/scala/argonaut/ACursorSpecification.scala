package argonaut

import org.scalacheck._, Prop._, Arbitrary._, Gen._
import Data._
import Argonaut._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

object ACursorSpecification extends Specification with ScalaCheck {

  def is = "ACursor" ^
    "History must reflect success after single step." ! prop((j: Json) => {
      forAll((op: TestOp) => {
        val r = step(j.acursor, op)
        if (r.succeeded)
          r.history.head.forall(h => h.isReattempt || h.succeeded)
        else
          r.history.head.exists(_.failed)
      })
    }) ^
    "History must reflect success after multiple steps." ! prop((j: Json) => {
      forAll((op: List[TestOp]) => {
        val r = op.foldLeft(j.acursor)((acc, op) => step(acc, op))
        if (r.succeeded)
          r.history.head.forall(h => h.isReattempt || h.succeeded)
        else
          r.history.head.exists(_.failed)
      })
    }) ^
    "Nothing accept reattempt may occur after failure." ! prop((j: Json) => {
      forAll((op: List[TestOp]) => {
        val r = op.foldLeft(j.acursor)((acc, op) => step(acc, op))
        r.history.toList.inits.toList.forall(paths => paths match {
          case init :+ penultimate :+ last =>
            last.succeeded || last.isReattempt || penultimate.isReattempt
          case init :+ last  =>
            last.succeeded || last.isReattempt || init.isEmpty
          case _ =>
            true
        })
      })
    })

  def step(start: ACursor, op: TestOp): ACursor =
    op match {
      case First =>
        start.first
      case Last =>
        start.last
      case Down =>
        if (start.focus.map(_.isObject).getOrElse(false))
          withField(start, _.downField(_))
        else if (start.focus.map(_.isArray).getOrElse(false))
          start.downArray
        else
          start
      case Up =>
        start.up
      case Left =>
        start.left
      case Right =>
        start.right
      case Sibling =>
        withField(start, _.field(_))
      case Delete =>
        start.delete
      case Set(j: Json) =>
        start.set(j)
      case Reattempt =>
        start.reattempt
    }

  def field(j: Json): Option[JsonField] =
    j.obj.flatMap(_.fields.headOption)

  def withField(a: ACursor, f: (ACursor, JsonField) => ACursor): ACursor =
    a.focus.flatMap(c => field(c).map(field => f(a, field))).getOrElse(a)


  trait TestOp
  case object Reattempt extends TestOp
  case object First extends TestOp
  case object Sibling extends TestOp
  case object Last extends TestOp
  case object Down extends TestOp
  case object Up extends TestOp
  case object Left extends TestOp
  case object Right extends TestOp
  case object Delete extends TestOp
  case class Set(j: Json) extends TestOp

  implicit val ArbitraryTestOp: Arbitrary[TestOp] =
    Arbitrary(Gen.frequency(
      (9, Gen.oneOf(Down, Up, Left, Right, Delete)),
      (1, arbitrary[Json] map (Set))
    ))


}
