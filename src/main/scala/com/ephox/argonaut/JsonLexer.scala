package com.ephox.argonaut

import util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import util.parsing.combinator.token.StdTokens

class JsonLexer extends StdLexical with StdTokens {
//    override def token: Parser[Token] = FIX lexical parser


// FIX Something like....  
//  def keyword = oneOf(List("true", "false", "null")) ^^ Keyword(_.toString)

}