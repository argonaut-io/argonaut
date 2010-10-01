package com.ephox.argonaut

sealed trait StringWrap {
  val value: String

  def parse: PossibleJson = JsonParser.parseOptional(value)
}

object StringWrap {
  implicit def StringStringWrap(s: String): StringWrap = new StringWrap {
    val value = s
  }
}
