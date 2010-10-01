package com.ephox.argonaut

sealed trait StringWrap {
  val value: String

  import Identity._

  def parse: PossibleJson = JsonParser.parseOptional(value).pjson
}

object StringWrap {
  implicit def StringStringWrap(s: String): StringWrap = new StringWrap {
    val value = s
  }
}
