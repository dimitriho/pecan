package fr.idho.pecan.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.combinator.token.StdTokens

case class PhpScript()
abstract class Statement

case class InlineHtml(code: String) extends Statement
case class Block(statements: List[Statement]) extends Statement
case class ClassDeclaration(className: String, parent: Option[String], interfaces: List[String], members: List[ClassMemberDec], modifier: Option[String]) extends Statement
abstract class ClassMemberDec
case class AttributeDec(name: String, init: Option[Expression], modifiers: List[String]) extends ClassMemberDec
case class MethodDec(name: String, isRef: Boolean, arguments: List[FunctionDecArg], body: Block, modifiers: List[String]) extends ClassMemberDec
case class FunctionDec(name: String, isRef: Boolean, arguments: List[FunctionDecArg], body: Block) extends Statement

case class FunctionDecArg(name: String, typeHint: Option[String], isRef: Boolean, init: Option[Expression])

case class BreakStmt() extends Statement
case class ContinueStmt() extends Statement
case class ReturnStmt(expr: Expression) extends Statement
case class TryStmt(statement: Statement, catchStmts: List[CatchStmt]) extends Statement
case class CatchStmt(variable: Variable, exceptionType: String, statement: Statement) extends Statement

abstract class Expression extends Statement
case class Assignment(variable: Variable, expr: Expression) extends Expression
case class Variable(name: String) extends Expression

object PhpParser extends StandardTokenParsers {
  override val lexical = PhpScanner

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(phpScript)(tokens)
  }

  def parse(s: String, parser: Parser[Any]) = {
    val tokens = new lexical.Scanner(s)
    phrase(parser)(tokens)
  }

  def phpScript = statement*

  /**
   * Statements
   */
  def statement: Parser[Statement] = inlineHtml | block | classDeclaration | functionDeclaration | tryCatchStmt /* |
  	requireStmt | whileStmt | doStmt | forStmt | forEachStmt | switchStmt |
  	breakStmt | continueStmt | returnStmt | tryCatchStmt | throwStmt |
  	requireStmt | requireOnceStmt | includeStmt |
    includeOnceStmt | exprStmt | echoStmt | inlineHtml */

  def inlineHtml = elem("inline html", _.isInstanceOf[lexical.InlineHtml]) ^^ { case a => new InlineHtml(a.chars) }

  def block = "{" ~> rep(statement) <~ "}" ^^ { new Block(_) }

  /* Class declaration */
  def classDeclaration = opt(classMod) ~ "class" ~ ident ~ opt("extends" ~> ident) ~
    interfaces ~ members ^^
    { case classMod ~ "class" ~ className ~ parent ~ interfaces ~ members => new ClassDeclaration(className, parent, interfaces, members, classMod) }
  def interfaces = opt("implements" ~> rep1sep(ident, ",")) ^^ { _.getOrElse(Nil) }
  def members = "{" ~> rep(method | attribute) <~ "}"
  /* Class members */
  def attribute = (attributeMod*) ~ variable ~ opt("=" ~> expr) <~ ";" ^^ { case modifiers ~ variable ~ init => new AttributeDec(variable.name, init, modifiers) } //TODO: not all expr are valid
  def method = (methodMod*) ~ functionDeclaration ^^ { case a ~ b => new MethodDec(b.name, b.isRef, b.arguments, b.body, a) }

  /* Function declaration */
  def functionDeclaration = "function" ~> opt("&") ~ ident ~ functionDecArgs ~ block ^^ { case isRef ~ name ~ functionArgs ~ block => new FunctionDec(name, isRef.isDefined, functionArgs, block) }
  def functionDecArgs = "(" ~> repsep(arg, ",") <~ ")"

  def arg = opt(typeHint) ~ opt("&") ~ variable ~ opt("=" ~> expr) ^^ { case typeHint ~ isRef ~ variable ~ init => new FunctionDecArg(variable.name, typeHint, isRef.isDefined, init) }

  /* Try / catch */
  def tryCatchStmt = "try" ~> block ~ rep1(catchStmt) ^^ { case block ~ catchStmts => new TryStmt(block, catchStmts) }
  def catchStmt = "catch" ~ "(" ~> ident ~ variable ~ ")" ~ "{" ~ statement <~ "}" ^^
    { case exceptionType ~ variable ~ ")" ~ "{" ~ statement => new CatchStmt(variable, exceptionType, statement) }

  /**
   * Expressions
   */
  def expr: Parser[Expression] = assignment | variable
  /*(functionCall | numericLit | variableInc | stringLit |
      assignment | const | variable | instanciation | methodCall) ~
      opt(op | methodCall)*/
  def assignment = variable ~ "=" ~ expr ^^ { case a ~ "=" ~ b => new Assignment(a, b) }
  def variable = "$" ~> ident ^^ { new Variable(_) }

  def attributeMod = "public" | "protected" | "private" | "static" | "const"
  def methodMod = "public" | "protected" | "private" | "static" | "abstract" | "final"
  def classMod = "abstract" | "final"
  def keywords = "isset" | "list" | "class"

  def typeHint = "array" | ident

  def ifStmt = "if" ~ "(" ~ expr ~ ")" ~ statement ~ opt("else" ~ statement)
  def ternaryStmt = expr ~ "?" ~ expr ~ ":" ~ expr <~ ";"
  def whileStmt = "while" ~ "(" ~ expr ~ ")" ~ statement
  def doStmt = "do" ~ statement ~ "while" ~ "(" ~ expr ~ ")" ~ ";"
  def forStmt = "for" ~ "(" ~ expr ~ ";" ~ expr ~ ";" ~ expr ~ ")" ~ statement
  def forEachStmt = "foreach" ~ "(" ~ expr ~ "as" ~ variable ~ ")" ~ statement
  def switchStmt = "switch" ~ "(" ~ expr ~ ")" ~ "{" ~ caseStmt ~ "}"
  def caseStmt = ("case" ~ expr | "default") ~ ":" ~ opt(statement) ~ opt(breakStmt)
  def breakStmt = "break" ~ ";" ^^ { _ => new BreakStmt }
  def continueStmt = "continue" ~ ";" ^^ { _ => new ContinueStmt }
  def returnStmt = "return" ~> expr <~ ";" ^^ { new ReturnStmt(_) }
  def throwStmt = "throw" ~> expr <~ ";"
  def requireStmt = "require" ~> stringLit <~ ";"
  def requireOnceStmt = "require_once" ~> stringLit <~ ";"
  def includeStmt = "include" ~> stringLit <~ ";"
  def includeOnceStmt = "include_once" ~> stringLit <~ ";"
  def echoStmt = "echo" ~> expr <~ ";"
  def exprStmt = opt(expr) <~ ";"

  def instanciation = "new" ~> (rep("\\" ~ ident) | ident) ~ opt(callArgs)

  def functionCall = (rep("\\" ~ ident) | ident) ~ callArgs
  def callArgs = "(" ~ repsep(expr, ",") ~ ")"

  def op = ("." | "+" | "^" | "-" | "<" | ">" | ">>" | "<<" | "==" | "===" | "!=" | "!==" | "<>" | ">=" | "<=") ~ expr
  def methodCall = "->" ~> ident ~ callArgs
  def variableInc = ("++" | "--") ~ variable | variable ~ ("++" | "--")

  def const = ident | "__CLASS__" | "__DIR__" | "__FILE__" | "__LINE__" |
    "__FUNCTION__" | "__METHOD__" | "__NAMESPACE__"

  def interfaceDef = "interface" ~> ident ~ "extends" ~ rep1sep(ident, ",")
}

