package fr.idho.pecan.parser
import scala.util.parsing.combinator.token.StdTokens

trait PhpTokens extends StdTokens {
  case class InlineHtml(chars: String) extends Token {
    override def toString = "`" + chars + "'"
  }
}