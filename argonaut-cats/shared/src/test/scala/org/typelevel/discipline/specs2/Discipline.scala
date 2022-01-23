package org.typelevel.discipline
package specs2

import org.specs2.ScalaCheck
import org.specs2.SpecificationLike
import org.specs2.specification.core.Fragments
import org.specs2.scalacheck.Parameters

// https://github.com/typelevel/discipline-specs2/blob/a04b6b05a73df404cf96696cb9cf414fc440ee17/core/src/main/scala/org/discipline/discipline/specs2/Discipline.scala
trait Discipline extends ScalaCheck { self: SpecificationLike =>

  def checkAll(name: String, ruleSet: Laws#RuleSet)(implicit p: Parameters) = {
    s"""${ruleSet.name} laws must hold for ${name}""" ^ br ^ t ^
    Fragments.foreach(ruleSet.all.properties.toList) { case (id, prop) =>
      id ! check(prop, p, defaultFreqMapPretty) ^ br
    } ^ br ^ bt
  }

}
