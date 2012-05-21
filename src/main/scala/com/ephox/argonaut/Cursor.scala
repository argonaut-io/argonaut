package com.ephox
package argonaut

import scalaz._
import JsonLike._
import Lens._
import CostateT._

sealed trait Cursor {
  val parent: Option[Cursor]
  val lefts: List[Json]
  val focus: Json
  val rights:List[Json]

  def parentL: Option[Cursor |--> Cursor] =
    Cursor.parentL run this

  def leftsL: Cursor |--> List[Json] =
    Cursor.leftsL run this

  def focusL: Cursor |--> Json =
    Cursor.focusL run this

  def rightsL: Cursor |--> List[Json] =
    Cursor.rightsL run this

  def up: Option[Cursor] = {
    def replaceChildren(j: Json, js: List[Json]): Json =
      j.fold(
        j
      , _ => j
      , _ => j
      , _ => j
      , a => jArray[Json](js)
      , o => jObject[Json](o map (_._1) zip js)
      )
    parent map (Cursor.focusL mod (f => replaceChildren(f, lefts.reverse ::: focus :: rights), _))
  }

  def left: Option[Cursor] =
    lefts match {
      case Nil => None
      case h::t => Some(new Cursor {
        val parent = Cursor.this.parent
        val lefts = t
        val focus = h
        val rights = Cursor.this.focus :: Cursor.this.rights
      })
    }

  def right: Option[Cursor] =
    rights match {
      case Nil => None
      case h::t => Some(new Cursor {
        val parent = Cursor.this.parent
        val lefts = Cursor.this.focus :: Cursor.this.lefts
        val focus = h
        val rights = t
      })
    }

  def down: Option[Cursor] = {
    def children(j: Json): List[Json] =
      j.fold(
        Nil
      , _ => Nil
      , _ => Nil
      , _ => Nil
      , a => a
      , o => o map (_._2)
      )
    children(focus) match {
      case Nil => None
      case h::t => Some(new Cursor {
        val parent = Some(Cursor.this)
        val lefts = Nil
        val focus = h
        val rights = t
      })
    }
  }

}

object Cursor extends Cursors

trait Cursors {
  def parentL: Cursor @?> Cursor =
    PLens(_.parent map (w => Costate(z => new Cursor {
      val parent = Some(z)
      val lefts = w.lefts
      val focus = w.focus
      val rights = w.rights
    }, w)))

  val leftsL: Cursor @> List[Json] =
    Lens(w => Costate(z => new Cursor {
      val parent = w.parent
      val lefts = z
      val focus = w.focus
      val rights = w.rights
    }, w.lefts))

  def focusL: Cursor @> Json =
    Lens(w => Costate(z => new Cursor {
      val parent = w.parent
      val lefts = w.lefts
      val focus = z
      val rights = w.rights
    }, w.focus))

  def rightsL: Cursor @> List[Json] =
    Lens(w => Costate(z => new Cursor {
      val parent = w.parent
      val lefts = w.lefts
      val focus = w.focus
      val rights = z
    }, w.rights))

}
