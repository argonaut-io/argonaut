package com.ephox.argonaut

import util.parsing.combinator.Parsers

trait ParserTools { self: Parsers =>
  def choice[T](ps: List[Parser[T]]) = ps.reduceRight(_ ||| _)
}
