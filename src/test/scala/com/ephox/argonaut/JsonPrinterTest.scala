package com.ephox.argonaut

import org.scalacheck.Properties
import org.scalacheck.Prop._
import JsonPrinter._
import StringWrap._
import Data._

object JsonPrinterTest extends Properties("JsonPrinter") {
  property("json prints consistently, compact -> pretty") =
          forAll({(j: Json) =>
              pretty(j) == pretty(compact(j).parse.get)
            })
  
  property("json prints consistently, pretty -> compact") =
          forAll({(j: Json) =>
              compact(j) == compact(pretty(j).parse.get)
            })
}

