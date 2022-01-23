package argonaut

import org.scalacheck.Prop._
import Data._
import Argonaut._
import scalaz._
import Scalaz._

object CursorSpecification extends ArgonautSpec {
  def is = s2"""
  Cursor
    Json->Cursor->Json                         ${prop((j: Json) => (-(+j)) === j)}
    Json->Cursor->withFocus                    ${prop((j: Json) => (+j).focus === j)}
    withFocus on focus changes nothing         ${prop((c: Cursor) => c.withFocus(_ => c.focus) === c)}
    withFocus identity changes nothing         ${prop((c: Cursor) => c.withFocus(j => j) === c)}
    >-> aliases withFocus                      ${prop((c: Cursor, f: Json => Json) => (c withFocus f) === (c >-> f))}
    set gives focus                            ${prop((c: Cursor, j: Json) => (c set j).focus === j)}
    := aliases set                             ${prop((c: Cursor, j: Json) => (c set j) === (c := j))}
    lefts head is left                         ${prop((c: Cursor) => c.lefts forall (a => a.headOption == c.left.map(_.focus)))}
    rights head is right                       ${prop((c: Cursor) => c.rights forall (a => a.headOption == c.right.map(_.focus)))}
    first has no lefts                         ${prop((c: Cursor) => c.first forall (_.left.isEmpty))}
    last has no rights                         ${prop((c: Cursor) => c.last forall (_.right.isEmpty))}
    left->right                                ${prop((c: Cursor) => c.left forall (_.right exists (_ == c)))}
    right->left                                ${prop((c: Cursor) => c.right forall (_.left exists (_ == c)))}
    downArray->up                              ${prop((c: Cursor) => c.downArray forall (_.up exists (_ == c)))}
    downArray                                  $downArray
    downAt constant                            $downAtConstant
    downAt constant true is same as down array $downAtConstantDownArray
    downAt                                     $downAt
    first                                      $first
    last                                       $last
    leftAt                                     $leftAt
    left                                       $left
    rightAt                                    $rightAt
    right                                      $right
    right is same as rightN(1)                 $rightOne
    rightN(0) is a no op                       $rightNoOp
    leftN                                      $leftN
    rightN                                     $rightN
    find                                       $find
  """

  def downArray = prop((x: Json, xs: List[Json]) => jArray(x :: xs).cursor.downArray.map(_.focus) must_== Some(x))

  def downAtConstant = prop((x: Json, xs: List[Json]) =>
    jArray(x :: xs).cursor.downAt(_ => true).map(_.focus) must_== Some(x)
  )

  def downAtConstantDownArray = prop((xs: List[Json]) =>
    jArray(xs).cursor.downAt(_ => true).map(_.focus) must_== jArray(xs).cursor.downArray.map(_.focus)
  )

  def downAt = prop((ys: List[Json], x: Json, xs: List[Json]) =>
    jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).map(_.focus) must_== Some(x))

  def first = prop((y: Json, ys: List[Json], x: Json, xs: List[Json]) =>
    jArray((y :: ys) ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.first).map(_.focus) must_== Some(y))

  def last = prop((ys: List[Json], x: Json, xs: List[Json], z: Json) =>
    jArray(ys ::: (x :: xs) ::: List(z)).cursor.downAt(_ == x).flatMap(_.last).map(_.focus) must_== Some(z))

  def leftAt = prop((xx: Json, x: Json, xs: List[Json]) =>
    jArray(xx :: x :: xs).cursor.downN(1).flatMap(_.leftAt(_ => true)).map(_.focus) must_== Some(xx))

  def left = prop((xx: Json, x: Json, xs: List[Json]) =>
    jArray(xx :: x :: xs).cursor.downN(1).flatMap(_.left).map(_.focus) must_== Some(xx))

  def rightAt = prop((xx: Json, x: Json, xs: List[Json]) =>
    jArray(xx :: x :: xs).cursor.downArray.flatMap(_.rightAt(_ => true)).map(_.focus) must_== Some(x))

  def right = prop((x: Json, xs: List[Json]) =>
    jArray(x :: xs).cursor.downArray.flatMap(_.right).map(_.focus) must_== xs.headOption)

  def rightOne = prop((x: Json, xs: List[Json]) =>
    jArray(x :: xs).cursor.downArray.flatMap(_.right).map(_.focus) must_==
      jArray(x :: xs).cursor.downArray.flatMap(_.rightN(1)).map(_.focus))

  def rightNoOp = prop((ys: List[Json], x: Json, xs: List[Json]) =>
    jArray(x :: xs).cursor.downArray.map(_.focus) must_==
      jArray(x :: xs).cursor.downArray.flatMap(_.rightN(0)).map(_.focus))

  def leftN = prop((ys: List[Json], x: Json, xs: List[Json]) =>
    !ys.contains(x) ==> {
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.leftN(Math.max(ys.size, 1))).map(_.focus) must_== ys.headOption
    }
  )

  def rightN = prop((ys: List[Json], x: Json, xs: List[Json]) =>
    !ys.contains(x) ==> {
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.rightN(Math.max(xs.size, 1))).map(_.focus) must_== xs.lastOption
    }
  )

  def find = prop((x: Json, xs: List[Json]) =>
    jArray(x :: xs).cursor.downArray.flatMap(_.find(_ => true)).map(_.focus) === Some(x))
}
