package cats.ext.std

import cats._
import cats.syntax.eq._
import cats.syntax.show._

/**
  * Created by luissanchez on 03/02/2016.
  */
trait TupleInstances {

  implicit def Tuple2Eq[A, B](implicit EA: Eq[A], EB: Eq[B]): Eq[(A, B)] = new Eq[(A, B)] {
    override def eqv(x: (A, B), y: (A, B)): Boolean = x._1 === y._1 && x._2 === y._2
  }

  implicit def Tuple2Show[A, B](implicit SA: Show[A], SB: Show[B]): Show[(A, B)] = new Show[(A, B)] {
    override def show(f: (A, B)): String = "(" + f._1.show + ", " + f._2.show + "}"
  }

  implicit def Tuple3Eq[A, B, C](implicit EA: Eq[A], EB: Eq[B], EC: Eq[C]): Eq[(A, B, C)] = new Eq[(A, B, C)] {
    override def eqv(x: (A, B, C), y: (A, B, C)): Boolean = x._1 === y._1 && x._2 === y._2 && x._3 === y._3
  }

  implicit def Tuple3Show[A, B, C](implicit SA: Show[A], SB: Show[B], SC: Show[C]): Show[(A, B, C)] = new Show[(A, B, C)] {
    override def show(f: (A, B, C)): String = "(" + f._1.show + ", " + f._2.show + ", " + f._3.show + "}"
  }
}

object tuple extends TupleInstances
