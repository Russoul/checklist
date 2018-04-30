package me.russoul

import scala.collection.immutable.HashMap

import CheckListInterpreter._
import CheckListAST._

object BuiltinFunctions {


  type BuiltinFunc = List[String] => Either[ErrString,String]


  def parseBoolean(str : String) : Option[Boolean] = {
    str match{
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
  }

  def parseInt(str : String) : Option[Int] = {
    try{
      val i = java.lang.Integer.parseInt(str)
      Some(i)
    }catch{
      case _ : NumberFormatException => None
    }
  }

  def parseDouble(str : String) : Option[Double] = {
    try{
      val d = java.lang.Double.parseDouble(str)
      Some(d)
    }catch{
      case _ : NumberFormatException => None
    }
  }

  def unaryMinus() : BuiltinFunc ={
    args =>
      if(args.length != 1){
        Left("One argument is required")
      }else{
        try{
          val double = java.lang.Double.parseDouble(args.head)
          Right((-double).toString)
        }catch{
          case _ : NumberFormatException => Left(s"input to unary `-` must be a number got ${args.head}")
        }
      }

  }

  def plus() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val doubles = args.map(x => java.lang.Double.parseDouble(x))
          Right(doubles.sum.toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `+` must be numbers")
        }
      }

  }

  def minus() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val doubles = args.map(x => java.lang.Double.parseDouble(x))
          Right((doubles.head - doubles.tail.sum).toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `-` must be numbers")
        }
      }

  }

  def mult() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val doubles = args.map(x => java.lang.Double.parseDouble(x))
          Right(doubles.product.toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `*` must be numbers")
        }
      }

  }

  def div() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val doubles = args.map(x => java.lang.Double.parseDouble(x))
          Right((doubles.head / doubles.tail.product).toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `/` must be numbers")
        }
      }

  }

  def compare() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val doubles = args.map(x => java.lang.Double.parseDouble(x))
          Right((doubles.head == doubles.tail.product).toString)
        }catch{
          case _ : NumberFormatException =>
            Right((args.head == args.last).toString)
        }
      }

  }

  def compareNot() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val doubles = args.map(x => java.lang.Double.parseDouble(x))
          Right((doubles.head == doubles.tail.product).toString)
        }catch{
          case _ : NumberFormatException =>
            Right((args.head != args.last).toString)
        }
      }

  }

  def gt() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val d1 = java.lang.Double.parseDouble(args.head)
          val d2 = java.lang.Double.parseDouble(args.last)
          Right((d1 > d2).toString)
        }catch{
          case _ : NumberFormatException => Right((args.head > args.last).toString)
        }
      }

  }
  def gte() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val d1 = java.lang.Double.parseDouble(args.head)
          val d2 = java.lang.Double.parseDouble(args.last)
          Right((d1 >= d2).toString)
        }catch{
          case _ : NumberFormatException => Right((args.head >= args.last).toString)
        }
      }

  }

  def lt() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val d1 = java.lang.Double.parseDouble(args.head)
          val d2 = java.lang.Double.parseDouble(args.last)
          Right((d1 < d2).toString)
        }catch{
          case _ : NumberFormatException => Right((args.head < args.last).toString)
        }
      }

  }

  def lte() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        try{
          val d1 = java.lang.Double.parseDouble(args.head)
          val d2 = java.lang.Double.parseDouble(args.last)
          Right((d1 <= d2).toString)
        }catch{
          case _ : NumberFormatException => Right((args.head <= args.last).toString)
        }
      }

  }

  def and() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        (for(a <- parseBoolean(args.head); b <- parseBoolean(args.last)) yield (a && b).toString) match {
          case None => Left(s"Boolean values is required `${args.head}` and `${args.last}`")
          case Some(ok) => Right(ok)
        }
      }

  }

  def or() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        (for(a <- parseBoolean(args.head); b <- parseBoolean(args.last)) yield (a || b).toString) match {
          case None => Left(s"Boolean values is required `${args.head}` and `${args.last}`")
          case Some(ok) => Right(ok)
        }
      }

  }

  def not() : BuiltinFunc ={
    args =>
      if(args.length != 1){
        Left("One argument is required")
      }else{
        (for(a <- parseBoolean(args.head)) yield (!a).toString) match {
          case None => Left(s"Boolean value is required, found `${args.head}`")
          case Some(ok) => Right(ok)
        }
      }

  }

  def modulo() : BuiltinFunc = {
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        (for(a <- parseInt(args.head); b <- parseInt(args.last)) yield (a % b).toString) match {
          case None => Left(s"Int values is required, found `${args.head}` and `${args.tail}`")
          case Some(ok) => Right(ok)
        }
      }
  }


  //all unary operators are prefix(postfix implementation should not be hard, can be done if needed)
  //make sure `opSymbols` contains the required symbols
  val builtinFunc = List( //all names must be unique
    BuiltinFuncObj("+", plus(), 2, Some(AssociativityLeft), 4), //name, function, arity, associativity if binary, precedence(only for binary ops)
    BuiltinFuncObj("-", minus(), 2, Some(AssociativityLeft), 4),
    BuiltinFuncObj("*", mult(), 2, Some(AssociativityLeft), 6),
    BuiltinFuncObj("/", div(), 2, Some(AssociativityLeft), 6),
    BuiltinFuncObj("==", compare(), 2, Some(AssociativityNone), 3),
    BuiltinFuncObj("!=", compareNot(), 2, Some(AssociativityNone), 3),
    BuiltinFuncObj(">", gt(), 2, Some(AssociativityNone), 3),
    BuiltinFuncObj(">=", gte(), 2, Some(AssociativityNone), 3),
    BuiltinFuncObj("<", lt(), 2, Some(AssociativityNone), 3),
    BuiltinFuncObj("<=", lte(), 2, Some(AssociativityNone), 3),
    BuiltinFuncObj("&&", and(), 2, Some(AssociativityNone), 2),
    BuiltinFuncObj("||", or(), 2, Some(AssociativityLeft), 1),
    BuiltinFuncObj("%", modulo(), 2, Some(AssociativityLeft), 6),
    BuiltinFuncObj("unary_!", not(), 1, None, 0),
    BuiltinFuncObj("unary_-", unaryMinus(), 1, None, 0))

}
