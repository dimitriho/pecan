package fr.idho.pecan.parser
import scala.util.parsing.input.Positional

case class PhpScript(statements: List[Statement]) extends Positional

case class Identifier(name: String) extends Positional
case class QualifiedName(name: String, path: List[String], full: Boolean) extends Positional

/**
 * Statements
 */
abstract class Statement extends Positional

case class EmptyStatement() extends Statement
case class InlineHtml(code: String) extends Statement
case class Block(statements: List[Statement]) extends Statement
case class ClassDeclaration(className: Identifier,
                            parent: Option[Identifier],
                            interfaces: List[Identifier],
                            members: List[ClassMemberDec],
                            modifier: Option[Identifier]) extends Statement
abstract class ClassMemberDec extends Positional
case class AttributeDec(name: Variable,
                        init: Option[Expression],
                        modifiers: List[Identifier]) extends ClassMemberDec
case class MethodDec(name: Identifier,
                     isRef: Boolean,
                     arguments: List[FunctionDecArg],
                     body: Block,
                     modifiers: List[Identifier]) extends ClassMemberDec
case class FunctionDec(name: Identifier,
                       isRef: Boolean,
                       arguments: List[FunctionDecArg],
                       body: Block) extends Statement
case class FunctionDecArg(name: Variable,
                          typeHint: Option[Identifier],
                          isRef: Boolean,
                          init: Option[Expression]) extends Positional

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
                     exceptionType: Identifier,
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
case class FunctionCall(function: QualifiedName,
                        arguments: List[Expression]) extends Expression
case class TernaryExpression(condition: Expression,
                             trueExpression: Option[Expression],
                             falseExpression: Expression) extends Expression
case class Instanciation(classname: QualifiedName,
                         arguments: List[Expression]) extends Expression