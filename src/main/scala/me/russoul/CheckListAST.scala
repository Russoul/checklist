package me.russoul

import me.russoul.BuiltinFunctions.BuiltinFunc

import scala.util.parsing.input.Positional

object CheckListAST{
  case class StringInterpolator(index : Int, expr: Expr)


  sealed trait Expr extends Positional
  sealed trait FuncObj extends Expr //used for custom and builtin functions


  case class StringInterpolatorExpr(e : Expr) extends Expr //used for disambiguation
  //case class BoundApplication(app : Application) extends Expr //special mark, telling the parser that this application must not participate in operator precedence/assoc rules resolution

  sealed trait Associativity
  object AssociativityLeft extends Associativity
  object AssociativityRight extends Associativity
  object AssociativityNone extends Associativity


  //all custom functions that are binary are none associative
  //associativity is only applicable to binary functions
  //and only those and unary ones can be used with operator notation
  case class BuiltinFuncObj(name : String, f : BuiltinFunc, arity : Int, assoc : Option[Associativity], precedence : Int) extends FuncObj

  //case class IntLit(i : Int) extends Lit
  //case class BoolLit(b : Boolean) extends Lit
  case class StringExpr(str : String, interpolators : List[StringInterpolator]) extends Expr
  case class ValueRef(name : String) extends Expr

  case class Conditional(cond : Expr, ifTrue : List[Expr], ifFalse : List[Expr]) extends Expr
  case class CheckList(name : String, exprs : List[Expr]) extends Expr
  case class Function(name : String, args : List[String], body : List[Expr]) extends FuncObj
  case class Binding(name : String, expr : Expr) extends Expr
  case class Application(name : String, args : List[Expr]) extends Expr

  case class Read(name : String) extends Expr
  case class Write(expr : Expr) extends Expr
  case class Entry(name : String, exprs : List[Expr]) extends Expr



}