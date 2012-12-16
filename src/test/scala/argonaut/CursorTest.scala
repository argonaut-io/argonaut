package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Argonaut._
import scalaz._, Scalaz._

object CursorTest extends Properties("Cursor") {
  property("Json->Cursor->Json") =
    forAll((j: Json) =>
      (-(+j)) === j
    )

  property("Json->Cursor->focus") =
    forAll((j: Json) =>
      (+j).focus === j
    )

  property("withFocus on focus changes nothing") =
    forAll((c: Cursor) =>
      c.withFocus(_ => c.focus) === c
    )

  property("withFocus identity changes nothing") =
    forAll((c: Cursor) =>
      c.withFocus(j => j) === c
    )

  property(">-> aliases withFocus") =
    forAll((c: Cursor, f: Json => Json) =>
      (c withFocus f) === (c >-> f)
    )

  property("set gives focus") =
    forAll((c: Cursor, j: Json) =>
      (c set j).focus === j
    )

  property(":= aliases set") =
    forAll((c: Cursor, j: Json) =>
      (c set j) === (c := j)
    )

  property("lefts head is left") =
    forAll((c: Cursor) =>
      c.lefts forall (a =>
        a.headOption === c.left.map(_.focus)
      )
    )

  property("rights head is right") =
    forAll((c: Cursor) =>
      c.rights forall (a =>
        a.headOption === c.right.map(_.focus)
      )
    )

  property("first has no lefts") =
    forAll((c: Cursor) =>
      c.first forall (_.left.isEmpty)
    )

  property("last has no rights") =
    forAll((c: Cursor) =>
      c.last forall (_.right.isEmpty)
    )

  property("left->right") =
    forAll((c: Cursor) =>
      c.left forall (_.right exists (_ === c))
    )

  property("right->left") =
    forAll((c: Cursor) =>
      c.right forall (_.left exists (_ === c))
    )

  property("downArray->up") =
    forAll((c: Cursor) =>
      c.downArray forall (_.up exists (_ === c))
    )
}
