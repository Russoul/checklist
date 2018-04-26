package me.russoul

import scala.collection.immutable.HashMap

import CheckListInterpreter._

object BuiltinFunctions {




  def sum() : BuiltinFunc ={
    args =>
      if(args.length < 2){
        Left("At least two arguments is required")
      }else{
        try{
          val ints = args.map(x => java.lang.Double.parseDouble(x))
          Right(ints.sum.toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `+` must be numbers")
        }
      }

  }

  def dif() : BuiltinFunc ={
    args =>
      if(args.length < 2){
        Left("At least two arguments is required")
      }else{
        try{
          val ints = args.map(x => java.lang.Double.parseDouble(x))
          Right((ints.head - ints.tail.sum).toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `-` must be numbers")
        }
      }

  }

  def mult() : BuiltinFunc ={
    args =>
      if(args.length < 2){
        Left("At least two arguments is required")
      }else{
        try{
          val ints = args.map(x => java.lang.Double.parseDouble(x))
          Right(ints.product.toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `*` must be numbers")
        }
      }

  }

  def div() : BuiltinFunc ={
    args =>
      if(args.length < 2){
        Left("At least two arguments is required")
      }else{
        try{
          val ints = args.map(x => java.lang.Double.parseDouble(x))
          Right((ints.head / ints.tail.product).toString)
        }catch{
          case _ : NumberFormatException => Left("inputs to `-` must be numbers")
        }
      }

  }

  def compare() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        Right((args.head == args.last).toString)
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
        Right((java.lang.Boolean.parseBoolean(args.head) && java.lang.Boolean.parseBoolean(args.last)).toString)
      }

  }

  def or() : BuiltinFunc ={
    args =>
      if(args.length != 2){
        Left("Two arguments is required")
      }else{
        Right((java.lang.Boolean.parseBoolean(args.head) || java.lang.Boolean.parseBoolean(args.last)).toString)
      }

  }

  def not() : BuiltinFunc ={
    args =>
      if(args.length != 1){
        Left("One argument is required")
      }else{
        Right((!java.lang.Boolean.parseBoolean(args.head)).toString)
      }

  }


  val builtinFunc = HashMap(
    ("+", sum()),
    ("-", dif()),
    ("*", mult()),
    ("/", div()),
    ("==", compare()),
    (">", gt()),
    (">=", gte()),
    ("<", lt()),
    ("<=", gte()),
    ("&&", and()),
    ("||", or()),
    ("!", not()))

}
