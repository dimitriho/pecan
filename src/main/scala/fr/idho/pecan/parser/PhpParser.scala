package fr.idho.pecan.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.lexical.StdLexical

class PhpParser(val scanner: PhpScanner = new PhpScanner)
  extends StandardTokenParsers {

  override val lexical = scanner

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(phpScript)(tokens)
  }

  def parse(s: String, parser: Parser[Any]) = {
    val tokens = new lexical.Scanner(s)
    phrase(parser)(tokens)
  }

  def phpScript = positioned(rep(statement) ^^ { new PhpScript(_) })

  /**
   * Statements
   */
  def statement: Parser[Statement] = inlineHtml | block | classDeclaration |
    functionDeclaration | tryCatchStmt | ifStmt | whileStmt | doWhileStmt |
    forStmt | forEachStmt | breakStmt | continueStmt | switchStmt |
    expr <~ ";" | positioned(";" ^^ (_ => new EmptyStatement))
  /* |
  	requireStmt |
  	requireStmt | requireOnceStmt | includeStmt |
    includeOnceStmt | exprStmt | echoStmt | inlineHtml */

  def inlineHtml = positioned(elem("inline html", _.isInstanceOf[lexical.InlineHtml]) ^^
    { case a => new InlineHtml(a.chars) })

  def block = positioned("{" ~> rep(statement) <~ "}" ^^ { new Block(_) })

  /* Class declaration */
  def classDeclaration = positioned(opt(classMod) ~ "class" ~ identifier ~
    opt("extends" ~> identifier) ~ interfaces ~ members ^^
    {
      case classMod ~ "class" ~ className ~ parent ~ interfaces ~ members =>
        new ClassDeclaration(className, parent, interfaces, members, classMod)
    })
  def interfaces = opt("implements" ~> rep1sep(identifier, ",")) ^^
    { _.getOrElse(Nil) }
  def members = "{" ~> rep(method | attribute) <~ "}"
  /* Class members */
  def attribute = positioned((attributeMod*) ~ variable ~ opt("=" ~> expr) <~ ";" ^^
    {
      case modifiers ~ variable ~ init =>
        new AttributeDec(variable, init, modifiers)
    }) //TODO: not all expr are valid
  def method = positioned((methodMod*) ~ functionDeclaration ^^
    { case a ~ b => new MethodDec(b.name, b.isRef, b.arguments, b.body, a) })

  /* Function declaration */
  def functionDeclaration = positioned("function" ~> opt("&") ~ identifier ~ functionDecArgs ~ block ^^
    {
      case isRef ~ name ~ functionArgs ~ block =>
        new FunctionDec(name, isRef.isDefined, functionArgs, block)
    })
  def functionDecArgs = "(" ~> repsep(functionDecArg, ",") <~ ")"
  def functionDecArg = positioned(opt(typeHint) ~ opt("&") ~ variable ~ opt("=" ~> expr) ^^
    {
      case typeHint ~ isRef ~ variable ~ init =>
        new FunctionDecArg(variable, typeHint, isRef.isDefined, init)
    })

  /* Try / catch */
  def tryCatchStmt = positioned("try" ~> block ~ rep1(catchStmt) ^^
    { case block ~ catchStmts => new TryStmt(block, catchStmts) })
  def catchStmt = positioned("catch" ~ "(" ~> identifier ~ variable ~ ")" ~ "{" ~ statement <~ "}" ^^
    {
      case exceptionType ~ variable ~ ")" ~ "{" ~ statement =>
        new CatchStmt(variable, exceptionType, statement)
    })

  /* Control structures */
  def ifStmt = positioned("if" ~ "(" ~> expr ~ ")" ~ statement ~ opt("else" ~> statement) ^^
    { case expr ~ ")" ~ stmt1 ~ stmt2 => new IfStmt(expr, stmt1, stmt2) })
  def whileStmt = positioned("while" ~ "(" ~> (expr <~ ")") ~ statement ^^
    { case expr ~ stmt => new WhileStmt(expr, stmt) })
  def doWhileStmt = positioned("do" ~> (statement <~ "while" ~ "(") ~ expr <~ ")" ~ ";" ^^
    { case stmt ~ expr => new DoWhileStmt(expr, stmt) })
  def forStmt = positioned("for" ~ "(" ~> (repsep(expr, ",") <~ ";") ~ (expr <~ ";") ~
    (repsep(expr, ",") <~ ")") ~ statement ^^
    { case init ~ cond ~ incr ~ stmt => new ForStmt(init, cond, incr, stmt) })
  def forEachStmt = positioned("foreach" ~ "(" ~> (expr <~ "as") ~ opt(variable <~ "=>") ~
    (variable <~ ")") ~ statement ^^
    { case expr ~ key ~ variable ~ stmt => new ForeachStmt(expr, key, variable, stmt) })
  def breakStmt = positioned("break" ~ ";" ^^ { _ => new BreakStmt })
  def continueStmt = positioned("continue" ~ ";" ^^ { _ => new ContinueStmt })
  def switchStmt = positioned("switch" ~ "(" ~> (expr <~ ")" ~ "{") ~ rep(caseStmt | defaultCaseStmt) <~ "}" ^^
    {
      case expr ~ caseStmts => new SwitchStmt(expr, caseStmts)
    })
  def defaultCaseStmt = positioned("default" ~ ":" ~> statement ^^ { new DefaultCaseStmt(_) })
  def caseStmt = positioned("case" ~> expr ~ ":" ~ statement ^^
    {
      case expr ~ ":" ~ stmt => new CaseStmt(expr, stmt)
    })

  /**
   * Expressions
   */
  def expr: Parser[Expression] = "(" ~> expr <~ ")" | literal | assignment |
    variable | languageContruct | ternaryExpr | functionCall | instanciation
  /*(| methodCall) ~
      opt(op | methodCall)*/
  def literal = positioned(
    numericLit ^^ { new NumberExpr(_) }
      | stringLit ^^ { new StringExpr(_) }
      | "null" ^^ { _ => new NilExpr }
      | "false" ^^ { _ => new BooleanExpr(false) }
      | "true" ^^ { _ => new BooleanExpr(true) }
      | (ident | "__CLASS__" | "__DIR__" | "__FILE__" | "__LINE__"
        | "__FUNCTION__" | "__METHOD__" | "__NAMESPACE__") ^^ { new Constant(_) })

  def assignment = positioned((variable <~ "=") ~ expr ^^
    { case a ~ b => new Assignment(a, b) })
  def variable = positioned("$" ~> ident ^^ { new Variable(_) })
  def languageContruct = positioned(("die" |
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
      case name ~ expr =>
        new FunctionCall(new QualifiedName(name, Nil, false), expr :: Nil)
    })
  def ternaryExpr = positioned((expr <~ "?") ~ (opt(expr) <~ ":") ~ expr <~ ";" ^^
    {
      case cond ~ trueExpr ~ falseExpr => new TernaryExpression(cond, trueExpr, falseExpr)
    })

  def functionCall = positioned(qualifiedName ~ callArgs ^^
    {
      case name ~ args => new FunctionCall(name, args)
    })

  def instanciation = positioned("new" ~> qualifiedName ~ opt(callArgs) ^^
    {
      case classname ~ args => new Instanciation(classname, args.getOrElse(Nil))
    })
  def callArgs = "(" ~> repsep(expr, ",") <~ ")"

  def identifier = positioned(ident ^^ { new Identifier(_) })
  def identifier(p: Parser[String]) = positioned(p ^^ { new Identifier(_) })

  def qualifiedName = positioned(opt("\\") ~ rep(ident <~ "\\") ~ ident ^^
    {
      case root ~ path ~ name => new QualifiedName(name, path, root.isDefined)
    })

  def attributeMod = identifier("public" | "protected" | "private" | "static" | "const")
  def methodMod = identifier("public" | "protected" | "private" | "static" | "abstract" | "final")
  def classMod = identifier("abstract" | "final")

  def typeHint = identifier("array") | identifier

  def op = ("." | "+" | "^" | "-" | "<" | ">" | ">>" | "<<" | "==" | "===" |
    "!=" | "!==" | "<>" | ">=" | "<=") ~ expr
  def methodCall = "->" ~> identifier ~ callArgs
  def variableInc = ("++" | "--") ~ variable | variable ~ ("++" | "--")

  def interfaceDef = "interface" ~> identifier ~ "extends" ~ rep1sep(identifier, ",")
}

