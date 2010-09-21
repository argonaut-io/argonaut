package com.ephox.argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties

object ParserTest extends Properties("Parser") {
  property("Addition commutes") = forAll((x: Int, y: Int) => x - y == y + x)
}
