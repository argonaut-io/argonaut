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
      c.downArray forall (_.up exists (_ === c))) ^ end
}
