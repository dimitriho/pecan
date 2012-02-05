package fr.idho.pecan.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical

case class PhpScript()

/**
 * Statements
 */
abstract class Statement

case class EmptyStatement() extends Statement
case class InlineHtml(code: String) extends Statement
case class Block(statements: List[Statement]) extends Statement
case class ClassDeclaration(className: String,
                            parent: Option[String],
                            interfaces: List[String],
                            members: List[ClassMemberDec],
                            modifier: Option[String]) extends Statement
abstract class ClassMemberDec
case class AttributeDec(name: String,
                        init: Option[Expression],
                        modifiers: List[String]) extends ClassMemberDec
case class MethodDec(name: String,
                     isRef: Boolean,
                     arguments: List[FunctionDecArg],
                     body: Block,
                     modifiers: List[String]) extends ClassMemberDec
case class FunctionDec(name: String,
                       isRef: Boolean,
                       arguments: List[FunctionDecArg],
                       body: Block) extends Statement
case class FunctionDecArg(name: String,
                          typeHint: Option[String],
                          isRef: Boolean,
                          init: Option[Expression])

/* Control structures */
case class IfStmt(condition: Expression,
                  trueStatement: Statement,
                  falseStatement: Option[Statement]) extends Statement
case class WhileStmt(condition: Expression,
                     statement: Statement) extends Statement
case class DoWhileStmt(condition: Expression,
                       statement: Statement) extends Statement
case class ForStmt(init: List[Expression],
                   condition: Expression,
                   increment: List[Expression],
                   statement: Statement) extends Statement
case class ForeachStmt(haystack: Expression,
                       key: Option[Variable],
                       value: Variable,
                       statement: Statement) extends Statement
case class BreakStmt() extends Statement
case class ContinueStmt() extends Statement
case class ReturnStmt(expr: Expression) extends Statement
case class TryStmt(statement: Statement,
                   catchStmts: List[CatchStmt]) extends Statement
case class CatchStmt(variable: Variable,
                     exceptionType: String,
                     statement: Statement) extends Statement
case class SwitchStmt(expr: Expression,
                      caseStmts: List[AbstractCaseStmt]) extends Statement
class AbstractCaseStmt extends Statement
case class DefaultCaseStmt(stmt: Statement) extends AbstractCaseStmt
case class CaseStmt(expr: Expression, stmt: Statement) extends AbstractCaseStmt

/**
 * Expressions
 */
abstract class Expression extends Statement
abstract class Literal extends Expression
case class NumberExpr(value: String) extends Literal
case class StringExpr(value: String) extends Literal
case class Constant(value: String) extends Literal
case class NilExpr() extends Literal
case class BooleanExpr(value: Boolean) extends Literal

case class Assignment(variable: Variable, expr: Expression) extends Expression
case class Variable(name: String) extends Expression
case class FunctionCall(function: String,
                        expr: List[Expression]) extends Expression

class PhpParser(val scanner: PhpScanner = new PhpScanner) extends StandardTokenParsers {
  override val lexical = scanner

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
  def statement: Parser[Statement] = inlineHtml | block | classDeclaration |
    functionDeclaration | tryCatchStmt | ifStmt | whileStmt | doWhileStmt |
    forStmt | forEachStmt | breakStmt | continueStmt | switchStmt | 
    expr <~ ";" | (";" ^^ (_ => new EmptyStatement))
  /* |
  	requireStmt |
  	requireStmt | requireOnceStmt | includeStmt |
    includeOnceStmt | exprStmt | echoStmt | inlineHtml */

  def inlineHtml = elem("inline html", _.isInstanceOf[lexical.InlineHtml]) ^^
    { case a => new InlineHtml(a.chars) }

  def block = "{" ~> rep(statement) <~ "}" ^^ { new Block(_) }

  /* Class declaration */
  def classDeclaration = opt(classMod) ~ "class" ~ ident ~
    opt("extends" ~> ident) ~ interfaces ~ members ^^
    {
      case classMod ~ "class" ~ className ~ parent ~ interfaces ~ members =>
        new ClassDeclaration(className, parent, interfaces, members, classMod)
    }
  def interfaces = opt("implements" ~> rep1sep(ident, ",")) ^^
    { _.getOrElse(Nil) }
  def members = "{" ~> rep(method | attribute) <~ "}"
  /* Class members */
  def attribute = (attributeMod*) ~ variable ~ opt("=" ~> expr) <~ ";" ^^
    {
      case modifiers ~ variable ~ init =>
        new AttributeDec(variable.name, init, modifiers)
    } //TODO: not all expr are valid
  def method = (methodMod*) ~ functionDeclaration ^^
    { case a ~ b => new MethodDec(b.name, b.isRef, b.arguments, b.body, a) }

  /* Function declaration */
  def functionDeclaration = "function" ~> opt("&") ~ ident ~ functionDecArgs ~ block ^^
    {
      case isRef ~ name ~ functionArgs ~ block =>
        new FunctionDec(name, isRef.isDefined, functionArgs, block)
    }
  def functionDecArgs = "(" ~> repsep(functionDecArg, ",") <~ ")"
  def functionDecArg = opt(typeHint) ~ opt("&") ~ variable ~ opt("=" ~> expr) ^^
    {
      case typeHint ~ isRef ~ variable ~ init =>
        new FunctionDecArg(variable.name, typeHint, isRef.isDefined, init)
    }

  /* Try / catch */
  def tryCatchStmt = "try" ~> block ~ rep1(catchStmt) ^^
    { case block ~ catchStmts => new TryStmt(block, catchStmts) }
  def catchStmt = "catch" ~ "(" ~> ident ~ variable ~ ")" ~ "{" ~ statement <~ "}" ^^
    {
      case exceptionType ~ variable ~ ")" ~ "{" ~ statement =>
        new CatchStmt(variable, exceptionType, statement)
    }

  /* Control structures */
  def ifStmt = "if" ~ "(" ~> expr ~ ")" ~ statement ~ opt("else" ~> statement) ^^
    { case expr ~ ")" ~ stmt1 ~ stmt2 => new IfStmt(expr, stmt1, stmt2) }
  def whileStmt = "while" ~ "(" ~> (expr <~ ")") ~ statement ^^
    { case expr ~ stmt => new WhileStmt(expr, stmt) }
  def doWhileStmt = "do" ~> (statement <~ "while" ~ "(") ~ expr <~ ")" ~ ";" ^^
    { case stmt ~ expr => new DoWhileStmt(expr, stmt) }
  def forStmt = "for" ~ "(" ~> (repsep(expr, ",") <~ ";") ~ (expr <~ ";") ~
    (repsep(expr, ",") <~ ")") ~ statement ^^
    { case init ~ cond ~ incr ~ stmt => new ForStmt(init, cond, incr, stmt) }
  def forEachStmt = "foreach" ~ "(" ~> (expr <~ "as") ~ opt(variable <~ "=>") ~
    (variable <~ ")") ~ statement ^^
    { case expr ~ key ~ variable ~ stmt => new ForeachStmt(expr, key, variable, stmt) }
  def breakStmt = "break" ~ ";" ^^ { _ => new BreakStmt }
  def continueStmt = "continue" ~ ";" ^^ { _ => new ContinueStmt }
  def switchStmt = "switch" ~ "(" ~> (expr <~ ")" ~ "{") ~ rep(caseStmt | defaultCaseStmt) <~ "}" ^^
    {
      case expr ~ caseStmts => new SwitchStmt(expr, caseStmts)
    }
  def caseStmt = "default" ~ ":" ~ statement ^^
    {
      case "default" ~ ":" ~ stmt => new DefaultCaseStmt(stmt)
    }
  def defaultCaseStmt = "case" ~> expr ~ ":" ~ statement ^^
    {
      case expr ~ ":" ~ stmt => new CaseStmt(expr, stmt)
    }

  /**
   * Expressions
   */
  def expr: Parser[Expression] = "(" ~> expr <~ ")" | literal | assignment |
    variable | languageContruct
  /*(functionCall | instanciation | methodCall) ~
      opt(op | methodCall)*/
  def literal = (
    numericLit ^^ { new NumberExpr(_) }
    | stringLit ^^ { new StringExpr(_) }
    | "null" ^^ { _ => new NilExpr }
    | "false" ^^ { _ => new BooleanExpr(false) }
    | "true" ^^ { _ => new BooleanExpr(true) }
    | (ident | "__CLASS__" | "__DIR__" | "__FILE__" | "__LINE__"
      | "__FUNCTION__" | "__METHOD__" | "__NAMESPACE__") ^^ { new Constant(_) })

  def assignment = (variable <~ "=") ~ expr ^^
    { case a ~ b => new Assignment(a, b) }
  def variable = "$" ~> ident ^^ { new Variable(_) }
  def languageContruct = ("die" |
    //    "echo" |
    "empty" |
    "exit" |
    "eval" |
    "include" |
    "include_once" |
    "isset" |
    //    "list" |
    "require" |
    "require_once" |
    "return" |
    "print" |
    "unset") ~
    expr ^^
    {
      case function ~ expr =>
        new FunctionCall(function, expr :: Nil)
    }

  def attributeMod = "public" | "protected" | "private" | "static" | "const"
  def methodMod = "public" | "protected" | "private" | "static" | "abstract" | "final"
  def classMod = "abstract" | "final"

  def typeHint = "array" | ident
  def ternaryExpr = (expr <~ "?") ~ (opt(expr) <~ ":") ~ expr <~ ";"

  def instanciation = "new" ~> (rep("\\" ~ ident) | ident) ~ opt(callArgs)

  def functionCall = (rep("\\" ~ ident) | ident) ~ callArgs
  def callArgs = "(" ~ repsep(expr, ",") ~ ")"

  def op = ("." | "+" | "^" | "-" | "<" | ">" | ">>" | "<<" | "==" | "===" |
    "!=" | "!==" | "<>" | ">=" | "<=") ~ expr
  def methodCall = "->" ~> ident ~ callArgs
  def variableInc = ("++" | "--") ~ variable | variable ~ ("++" | "--")

  def interfaceDef = "interface" ~> ident ~ "extends" ~ rep1sep(ident, ",")
}

