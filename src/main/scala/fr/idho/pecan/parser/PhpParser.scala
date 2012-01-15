package fr.idho.pecan.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.combinator.token.StdTokens

object PhpParser extends StandardTokenParsers {
  override val lexical = PhpScanner

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(phpScript)(tokens)
  }

  def phpScript = seq

  def inlineHtml = elem("inline html", _.isInstanceOf[lexical.InlineHtml])

  def attributeMod = keyword("public") | keyword("protected") | keyword("private") | keyword("static") | keyword("const")
  def methodMod = keyword("public") | keyword("protected") | keyword("private") | keyword("static") | keyword("abstract") | keyword("final")
  def classMod = keyword("abstract") | keyword("final")
  def keywords = keyword("isset") | keyword("list") | keyword("class")

  def classDef = opt(classMod) ~ keyword("class") ~ ident ~ parent ~ classBody
  def parent = opt(keyword("extends") ~> ident) ~ opt(keyword("implements") ~> rep1sep(ident, ","))
  def classBody = "{" ~> (member*) <~ "}"
  def member = method | attribute
  def attribute = (attributeMod*) ~ variableWithDefault
  def method = (methodMod*) ~ funDec
  
  def funDec = keyword("function") ~ opt("&") ~ ident ~ functionDecArgs ~ "{" ~ seq ~ "}"

  def functionDecArgs = "(" ~> repsep(arg, ",") <~ ")"
  def arg = opt(typeHint) ~ opt("&") ~ variableWithDefault

  def typeHint = keyword("array") | ident

  def variable = "$" ~ ident
  def variableWithDefault = opt("&") ~ "$" ~ ident ~ opt("=" ~ expr)

  def stmt: Parser[Any] = "{" ~ seq ~ "}" | expr | classDef | funDec | requireStmt | whileStmt | doStmt |
    forStmt | forEachStmt | switchStmt | breakStmt | continueStmt | returnStmt |
    tryCatchStmt | throwStmt | requireStmt | requireOnceStmt | includeStmt |
    includeOnceStmt | exprStmt | echoStmt | inlineHtml

  def seq = stmt+

  def ifStmt = keyword("if") ~ "(" ~ expr ~ ")" ~ stmt ~ "else" ~ stmt | "if" ~ "(" ~ expr ~ ")" ~ stmt
  def whileStmt = keyword("while") ~ "(" ~ expr ~ ")" ~ stmt
  def doStmt = keyword("do") ~ stmt ~ keyword("while") ~ "(" ~ expr ~ ")" ~ ";"
  def forStmt = keyword("for") ~ "(" ~ expr ~ ";" ~ expr ~ ";" ~ expr ~ ")" ~ stmt
  def forEachStmt = "foreach" ~ "(" ~ expr ~ "as" ~ variable ~ ")" ~ stmt
  def switchStmt = "switch" ~ "(" ~ expr ~ ")" ~ "{" ~ caseStmt ~ "}"
  def caseStmt = ("case" ~ expr | "default") ~ ":" ~ opt(stmt) ~ opt(breakStmt)
  def breakStmt = "break" ~ ";"
  def continueStmt = "continue" ~ ";"
  def returnStmt = "return" ~ expr ~ ";"
  def tryCatchStmt = "try" ~ "{" ~ stmt ~ "}" ~ rep1(catchStmt)
  def catchStmt = "catch" ~ "(" ~ ident ~ variable ~ ")" ~ "{" ~ stmt ~ "}"
  def throwStmt = "throw" ~ expr ~ ";"
  def requireStmt = "require" ~ stringLit ~ ";"
  def requireOnceStmt = "require_once" ~ stringLit ~ ";"
  def includeStmt = "include" ~ stringLit ~ ";"
  def includeOnceStmt = "include_once" ~ stringLit ~ ";"
  def echoStmt = "echo" ~ expr ~ ";"
  def exprStmt = opt(expr) ~ ";"

  def instanciation = "new" ~ (rep("\\" ~ ident) | ident) ~ opt(callArgs)

  def functionCall = (rep("\\" ~ ident) | ident) ~ callArgs
  def callArgs = "(" ~ repsep(expr, ",") ~ ")"

  def expr: Parser[Any] = (functionCall | numericLit | variableInc | stringLit | assignment | const | variable | instanciation |
      methodCall) ~ opt(op | methodCall)

  def assignment = variable ~ "=" ~ expr
  def op = ("." | "+" | "^" | "-" | "<" | ">" | ">>" | "<<" | "==" | "===" | "!=" | "!==" | "<>" |  ">=" | "<=") ~ expr
  def methodCall = "->" ~ ident ~ callArgs
  def variableInc = ("++" | "--") ~ variable | variable ~ ("++" | "--")
  
  def const = ident | "__CLASS__" | "__DIR__" | "__FILE__" | "__LINE__" |
    "__FUNCTION__" | "__METHOD__" | "__NAMESPACE__"

  def interfaceDef = keyword("interface") ~> ident ~ keyword("extends") ~ rep1sep(ident, ",")
}

