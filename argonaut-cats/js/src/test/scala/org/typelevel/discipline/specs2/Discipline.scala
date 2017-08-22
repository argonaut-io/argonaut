package org.typelevel.discipline
package specs2

import org.specs2.ScalaCheck
import org.specs2.SpecificationLike
import org.specs2.specification.core.Fragments
import org.specs2.scalacheck.Parameters

// https://github.com/typelevel/discipline/blob/v0.8/jvm/src/main/scala/specs2/Discipline.scala
trait Discipline extends ScalaCheck { self: SpecificationLike =>

  def checkAll(name: String, ruleSet: Laws#RuleSet)(implicit p: Parameters) = {
    s"""${ruleSet.name} laws must hold for ${name}""" ^ br ^
    Fragments.foreach(ruleSet.all.properties) { case (id, prop) =>
       id ! check(prop, p, defaultFreqMapPretty) ^ br
    }
  }

}
