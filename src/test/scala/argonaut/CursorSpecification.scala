package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Argonaut._
import org.specs2._, org.specs2.specification._
import org.specs2.matcher._
import scalaz._
import Scalaz._

object CursorSpecification extends Specification with ScalaCheck {
  def is = "Cursor" ^
    "Json->Cursor->Json" ! prop((j: Json) =>
      (-(+j)) === j) ^
    "Json->Cursor->focus" ! prop((j: Json) =>
      (+j).focus === j) ^
    "withFocus on focus changes nothing" ! prop((c: Cursor) =>
      c.withFocus(_ => c.focus) === c) ^
    "withFocus identity changes nothing" ! prop((c: Cursor) =>
      c.withFocus(j => j) === c) ^
    ">-> aliases withFocus" ! prop((c: Cursor, f: Json => Json) =>
      (c withFocus f) === (c >-> f)) ^
    "set gives focus" ! prop((c: Cursor, j: Json) =>
      (c set j).focus === j) ^
    ":= aliases set" ! prop((c: Cursor, j: Json) =>
      (c set j) === (c := j)) ^
    "lefts head is left" ! prop((c: Cursor) =>
      c.lefts forall (a =>
        a.headOption === c.left.map(_.focus)
      )) ^
    "rights head is right" ! prop((c: Cursor) =>
      c.rights forall (a =>
        a.headOption === c.right.map(_.focus)
      )) ^
    "first has no lefts" ! prop((c: Cursor) =>
      c.first forall (_.left.isEmpty)) ^
    "last has no rights" ! prop((c: Cursor) =>
      c.last forall (_.right.isEmpty)) ^
    "left->right" ! prop((c: Cursor) =>
      c.left forall (_.right exists (_ === c))) ^
    "right->left" ! prop((c: Cursor) =>
      c.right forall (_.left exists (_ === c))) ^
    "downArray->up" ! prop((c: Cursor) =>
      c.downArray forall (_.up exists (_ === c))) ^
    "downArray" ! prop((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.map(_.focus) must_== Some(x)) ^
    "downAt constant" ! prop((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downAt(_ => true).map(_.focus) must_== Some(x)) ^
    "downAt constant true is same as down array" ! prop((xs: List[Json]) =>
      jArray(xs).cursor.downAt(_ => true).map(_.focus) must_== jArray(xs).cursor.downArray.map(_.focus)) ^
    "downAt" ! prop((ys: List[Json], x: Json, xs: List[Json]) =>
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).map(_.focus) must_== Some(x)) ^
    "first" ! prop((y: Json, ys: List[Json], x: Json, xs: List[Json]) =>
      jArray((y :: ys) ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.first).map(_.focus) must_== Some(y)) ^
    "last" ! prop((ys: List[Json], x: Json, xs: List[Json], z: Json) =>
      jArray(ys ::: (x :: xs) ::: List(z)).cursor.downAt(_ == x).flatMap(_.last).map(_.focus) must_== Some(z)) ^
    "rightAt" ! prop((xx: Json, x: Json, xs: List[Json]) =>
      jArray(xx :: x :: xs).cursor.downArray.flatMap(_.rightAt(_ => true)).map(_.focus) must_==  Some(x)) ^
    "right" ! prop((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.flatMap(_.right).map(_.focus) must_== xs.headOption) ^
    "right is same as rightN(1)" ! prop((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.flatMap(_.right).map(_.focus) must_==
       jArray(x :: xs).cursor.downArray.flatMap(_.rightN(1)).map(_.focus)) ^
    "rightN(0) is a no op" ! prop((ys: List[Json], x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.map(_.focus) must_==
       jArray(x :: xs).cursor.downArray.flatMap(_.rightN(0)).map(_.focus)) ^
    "leftN" ! prop((ys: List[Json], x: Json, xs: List[Json]) => !ys.contains(x) ==> {
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.leftN(Math.max(ys.size, 1))).map(_.focus) must_==  ys.headOption }) ^
    "rightN" ! prop((ys: List[Json], x: Json, xs: List[Json]) => !ys.contains(x) ==> {
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.rightN(Math.max(xs.size, 1))).map(_.focus) must_==  xs.lastOption }) ^
    "find" ! prop((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.flatMap(_.find(_ => true)).map(_.focus) === Some(x)) ^ end



}
