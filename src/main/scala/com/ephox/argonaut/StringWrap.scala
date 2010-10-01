package com.ephox.argonaut

import util.parsing.input.CharSequenceReader

sealed trait StringWrap {
  val value: String

  import Identity._

  def parse[X](success: Json => X, error: String => X, failure: String => X) = {
    val p = new JsonParser
    val r = new CharSequenceReader(value)
    p.jvalue(r) match {
      case p.Success(j, _) => success(j)
      case p.Error(e, _) => error(e)
      case p.Failure(e, _) => failure(e)
    }
  }

  def parseIgnoreErrorType[X](success: Json => X, nosuccess: String => X) =
    parse(success, nosuccess, nosuccess)

  def parse = {
    val p = new JsonParser
    val r = new CharSequenceReader(value)
    p.jvalue(r)
  }

  def parseTo[T](i: Json => T) = parse.map(i.apply(_))

  def parseToOrDie[T](i: Json => T): T = {
    val r = parseTo(i)
    if (r.successful) r.get else error("Unsuccessful parse result: " + r)
  }
  
  def pparse: PossibleJson = parseIgnoreErrorType(Some(_), _ => None).pjson
}

object StringWrap {
  implicit def StringStringWrap(s: String): StringWrap = new StringWrap {
    val value = s
  }
}
