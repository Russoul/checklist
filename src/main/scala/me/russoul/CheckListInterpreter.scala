package me.russoul

import java.io.{BufferedInputStream, InputStreamReader, OutputStream, PrintStream, PrintWriter}
import java.util.Scanner

import CheckListAST._

import scala.collection.mutable
import scala.collection.immutable
import BuiltinFunctions._

object CheckListInterpreter {

  //TODO unified tabulation
  //TODO TESTS
  //TODO do not skip newlines in checklist ?
  //TODO app
  //TODO easy switch between debug and release mode (no debug info is printed in release) see `log` parser
  //TODO make sure all TODOs are checked before handing in
  //TODO colored output ?   :P

  type ErrString = String
  final val TAB_SIZE = 3


  type BindingEnv = mutable.ListBuffer[mutable.HashMap[String, String]]



  def envContains(env : BindingEnv, str : String) : Boolean = {
    if(env.isEmpty) throw new Exception("trying to work with empty environment")
    for(hash <- env){ //reverse not required here
      if(hash.contains(str)) return true
    }

    false
  }

  def envGet(env : BindingEnv, str : String) : String = {
    if(env.isEmpty) throw new Exception("trying to work with empty environment")
    for(hash <- env.reverse){
      hash.get(str) match{
        case Some(x) => return x
        case None => ()
      }
    }

    throw new Exception(s"environment does not contain ${str}")
  }

  def envGetOption(env : BindingEnv, str : String) : Option[String] = {
    if(env.isEmpty) throw new Exception("trying to work with empty environment")
    for(hash <- env.reverse){
      hash.get(str) match{
        case ok@Some(_) => return ok
        case None => ()
      }
    }

    None
  }

  def envPush(env : BindingEnv) : Unit = {
    env += new mutable.HashMap[String, String]
  }

  def envPop(env : BindingEnv) : Unit = {
    if(env.isEmpty) throw new Exception("trying to pop empty env")
    env.remove(env.length - 1)
  }

  def envPut(env : BindingEnv, str : String, value : String) : Unit = {
    if(env.isEmpty) throw new Exception("trying work with empty environment")
    env.last.put(str, value)
  }

  def findFunctions(checkList: CheckList) : List[Function] = {
    def find(list : List[Expr]): List[Function] = { list match {
        case (x: Function) :: xs => x :: find(xs)
        case _ :: xs => find(xs)
        case _ => Nil
      }
    }

    find(checkList.exprs)
  }






  class Interpreter(val checklist: CheckList, val out : PrintStream, val in : Scanner){
    def interpret(): Either[ErrString, String] ={
      val funcs = findFunctions(checklist)

      if(funcs.distinct.length != funcs.length){
        return Left("duplicate function names found !")
      }

      val funcHashmap = immutable.HashMap[String, FuncObj](funcs.map(x => (x.name, x)) ++ builtinFunc.map(x => (x.name, x)) : _*)

      val bindingEnv = new mutable.ListBuffer[mutable.HashMap[String, String]]
      envPush(bindingEnv)



      var str = "##" + checklist.name + "\n"
      val errStr = s"\nIn checklist `${checklist.name}`"

      for(expr <- checklist.exprs){
        expr match{
          case _ : Function => () //do nothing
          case expr : Binding =>
            handleBinding(bindingEnv, expr, funcHashmap) match{
              case Some(err) => return Left(err + errStr)
              case None => () //ok
            }
          case expr : Application =>
            handleApplication(bindingEnv, expr, funcHashmap, tabs = 0) match{
              case Left(err) => return Left(err + errStr)
              case Right(ok) => str += ok + newLineStr(true)
            }
          case expr : StringExpr =>
            handleStringExpr(bindingEnv, 0, newLine = false, expr, funcHashmap) match{
              case Left(err) => return Left(err + errStr)
              case Right(ok) => str += ok + newLineStr(true)
            }
          case expr : Entry =>
            handleEntry(bindingEnv, 0, expr, funcHashmap) match {
              case Left(err) => return Left(err + errStr)
              case Right(ok) => str += ok + "\n"
            }
          case expr : ValueRef =>
            handleValueRef(bindingEnv, expr) match{
              case Left(err) => return Left(err + errStr)
              case Right(ok) => str += ok + "\n"
            }
          case expr : Conditional =>
            handleConditional(0, bindingEnv, expr, funcHashmap) match{
              case Left(err) => return Left(err + errStr)
              case Right(ok) => str += ok
            }
          case expr : Write =>
            handleWrite(bindingEnv, expr, funcHashmap) match{
              case Some(err) => return Left(err + errStr)
              case None => () //ok
            }
          case expr : Read =>
            handleRead(bindingEnv, expr, funcHashmap) match{
              case Some(err) => return Left(err + errStr)
              case None => () //ok
            }
          case _ => return Left(s"not yet implemented ${expr} in CheckList body")
        }
      }

      Right(str)

    }


    def handleStringInterpolatorExprs(env : BindingEnv, list : List[Expr], funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString,List[String]] = {
      list match{
        //case (i : IntLit) :: xs => for(rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield i.i.toString :: rest
        //case (b : BoolLit) :: xs => for(rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield b.b.toString :: rest
        case (s : StringExpr) :: xs => for(str <- handleStringExpr(env, 0, newLine = false, s, funcs); rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield str :: rest
        case (a : Application) :: xs => for(v <- handleApplication(env, a, funcs, tabs = 0); rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield v :: rest
        case (r : ValueRef) :: xs => for(v <- handleValueRef(env, r); rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield v :: rest
        case Nil => Right(Nil)
      }
    }

    def interpolateString(str : String, interpolation : List[(Int, String)]) : String = {
      interpolation match{
        case x :: xs =>
          val newStr = str.substring(0, x._1) + x._2 + str.substring(x._1, str.length)
          interpolateString(newStr, xs.map{ case (i,s) =>  (i + x._2.length, s)})
        case Nil => str
      }
    }

    def handleStringExpr(env : BindingEnv, tabs : Int, newLine : Boolean, lit : StringExpr, funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString, String] = {
      if(lit.interpolators.isEmpty){
        Right(fillTab(tabs) + lit.str + (if(newLine)"\n" else ""))
      }else{
        handleStringInterpolatorExprs(env, lit.interpolators.map(_.expr), funcs) match{
          case Left(err) => Left(err)
          case Right(ok) => Right(fillTab(tabs) + interpolateString(lit.str, lit.interpolators.map(x => x.index).zip(ok)) + (if(newLine)"\n" else ""))

        }
      }
    }

    def fillTab(tabs : Int) : String = {
      var str = ""
      for(_ <- 0 until TAB_SIZE*tabs) str += " "

      str
    }

    def handleEntry(env : BindingEnv, tabs : Int, entry : Entry, funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString, String] = {
      val str = fillTab(tabs) + "#" + entry.name + "\n"

      val errStr = s"\nin entry `${entry.name}`\n[${entry.pos}]\n${entry.pos.longString}"

      envPush(env)

      def handle(list : List[Expr]) : Either[ErrString, String] = {
        list match{
          case (x : StringExpr) :: xs =>
            for(done <- handleStringExpr(env, tabs + 1, newLine = xs.exists(!isErased(_)), x, funcs); other <- handle(xs)) yield done + other
          case (x : Application) :: xs => for(done <- handleApplication(env, x, funcs, tabs + 1); other <- handle(xs)) yield done + newLineStr(xs.exists(!isErased(_))) + other
          case (x : Binding) :: xs =>
            handleBinding(env, x, funcs) match{
              case Some(err) => Left(err+errStr)
              case None => for(other <- handle(xs)) yield other
            }
          case (x : ValueRef) :: xs =>
            handleValueRef(env, x) match{
              case Left(err) => Left(err+errStr)
              case Right(ok) => for(other <- handle(xs)) yield fillTab(tabs + 1) + ok + newLineStr(xs.exists(!isErased(_))) + other
            }
          case (x : Entry) :: xs =>
            for(e <- handleEntry(env, tabs + 1, x, funcs);rest <- handle(xs)) yield e + newLineStr(xs.exists(!isErased(_))) + rest
          case (x : Conditional) :: xs =>
            for(c <- handleConditional(tabs + 1, env, x, funcs); rest <- handle(xs)) yield c + newLineStr(xs.exists(!isErased(_))) + rest
          case (x : Write) :: xs =>
            handleWrite(env, x, funcs) match{
              case Some(err) => Left(err+errStr)
              case None => handle(xs)
            }
          case (x : Read) :: xs =>
            handleRead(env, x, funcs) match{
              case Some(err) => Left(err + errStr)
              case None => handle(xs)
            }

          case Nil => Right("")
          case x :: _ => Left(s"${x} is not implemented"+errStr)
        }
      }

      val result = handle(entry.exprs)
      envPop(env)
      for(_result <- result) yield str + _result
    }

    def handleValueRef(env : BindingEnv, ref : ValueRef) : Either[ErrString, String] = {
      envGetOption(env, ref.name) match{
        case None => Left(s"reference not found `${ref.name}`\n[${ref.pos}]\n${ref.pos.longString}")
        case Some(x) => Right(x)
      }
    }


    def newLineStr(newLine : Boolean): String = if(newLine) "\n" else ""

    def handleApplication(env : BindingEnv, apply: Application, funcs : immutable.HashMap[String, FuncObj], tabs : Int) : Either[ErrString, String] = {

      val errStr = s"\nIn application `${apply.name}`\n[${apply.pos}]\n${apply.pos.longString}"
      if(!funcs.contains(apply.name)){
        Left(s"function not found `${apply.name}`\n[${apply.pos}]\n${apply.pos.longString}" + errStr)
      }else{

        val func = funcs(apply.name)

        handleApplicationArgs(env, apply.args, funcs) match{
          case Left(err) => Left(err + errStr)
          case Right(args) =>


            func match{
              case func : BuiltinFuncObj =>
                func.f(args) match{
                  case Left(err) => Left(err + errStr)
                  case Right(ok) => Right(fillTab(tabs) + ok)
                }
              case func : Function =>
                envPush(env)

                if(func.args.length != apply.args.length) return Left(s"incorrect amount of arguments: ${apply.args.length}, required: ${func.args.length}" + errStr)

                for((value, name) <- args.zip(func.args)){
                  envPut(env, name, value)
                }

                def handle(exprs : List[Expr]) : Either[ErrString,String] = {
                  exprs match{
                    case (x : StringExpr) :: xs =>
                      for(res <- handleStringExpr(env, 0, newLine = xs.exists(!isErased(_)), x, funcs); other <- handle(xs)) yield fillTab(tabs) + res + other
                    /*case (x : BoolLit) :: xs =>
                      for(other <- handle(xs)) yield fillTab(tabs) + x.b.toString + newLineStr(newLine) + other
                    case (x : IntLit) :: xs =>
                      for(other <- handle(xs)) yield fillTab(tabs) + x.i.toString + newLineStr(newLine) + other*/
                    case (x : ValueRef) :: xs =>
                      for(ref <- handleValueRef(env, x); other <- handle(xs)) yield fillTab(tabs) + ref + newLineStr(xs.exists(!isErased(_))) + other
                    case (x : Application) :: xs =>
                      for(app <- handleApplication(env, x, funcs, tabs = 0); other <- handle(xs)) yield fillTab(tabs) + app + newLineStr(xs.exists(!isErased(_))) + other
                    case (cond : Conditional) :: xs =>
                      for(cond <- handleConditional(tabs, env, cond, funcs); other <- handle(xs)) yield {
                        cond + newLineStr(xs.exists(!isErased(_))) + other
                      }
                    case (x : Write) :: xs =>
                      handleWrite(env, x, funcs) match{
                        case Some(err) => Left(err)
                        case None =>
                          handle(xs)
                      }
                    case (x : Read) :: xs =>
                      handleRead(env, x, funcs) match{
                        case Some(err) => Left(err)
                        case None =>
                          handle(xs)
                      }
                    case Nil => Right("")
                    case x :: _ => Left(s"Unsupported element `${x}` in function  `${func.name}`")
                  }
                }

                val result = handle(func.body) match{
                  case Left(err) => Left(err + errStr)
                  case Right(ok) => Right(ok)
                }
                envPop(env)
                result
            }

        }



      }
    }


    def handleApplicationArgs(env : BindingEnv, args : List[Expr], funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString, List[String]] = {
      args match{
        //case (x : IntLit) :: xs => for(rest <- handleApplicationArgs(env, xs, funcs)) yield x.i.toString :: rest
        case (x : StringExpr) :: xs => for(str <- handleStringExpr(env, 0, newLine = false, x, funcs); rest <- handleApplicationArgs(env, xs, funcs)) yield str :: rest
        case (x : Application) :: xs => for(app <- handleApplication(env, x, funcs, tabs = 0); rest <- handleApplicationArgs(env, xs, funcs)) yield app :: rest
        case (x : ValueRef) :: xs => for(value <- handleValueRef(env, x); rest <- handleApplicationArgs(env, xs, funcs)) yield value :: rest
        case Nil => Right(Nil)
        case x :: _ => throw new Exception(s"cannot handle ${x}")
      }
    }

    def handleBinding(env : BindingEnv, binding : Binding, funcs : immutable.HashMap[String, FuncObj]) : Option[ErrString] = {
      val errStr = s"\nIn binding ${binding.name}\n[${binding.pos}]\n${binding.pos.longString}"
      binding.expr match{
        /*case x : IntLit => envPut(env, binding.name, x.i.toString); None
        case x : BoolLit => envPut(env, binding.name, x.b.toString); None*/
        case x : StringExpr => handleStringExpr(env, 0, newLine = false, x, funcs) match{
          case Left(err) => Some(err + errStr)
          case Right(str) => envPut(env, binding.name, str); None
        }
        case x : ValueRef =>
          handleValueRef(env, x) match{
            case Left(err) => Option(err + errStr)
            case Right(value) => envPut(env, binding.name, value); None
          }
        case x : Application =>
          handleApplication(env, x, funcs, tabs  = 0) match{
            case Left(err) => Option(err + errStr)
            case Right(ok) => envPut(env, binding.name, ok); None
          }
      }
    }

    def isErased(x : Expr) : Boolean = {
      x match{
        case Binding(_,_) => true
        case Write(_) => true
        case Read(_) => true
        case _ => false
      }
    }

    def handleConditionalBranch(tabs : Int, env: BindingEnv, body : List[Expr], funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString, String] = {
      envPush(env)


      def handle(list : List[Expr]) : Either[ErrString, String] = {
        list match{
          case (x : StringExpr) :: xs =>
            for(done <- handleStringExpr(env, tabs, newLine = xs.exists(!isErased(_)), x, funcs); other <- handle(xs)) yield done + other
          /*case (x : IntLit) :: xs =>
            for(other <- handle(xs)) yield fillTab(tabs) + x.i.toString + "\n" + other
          case (x : BoolLit) :: xs =>
            for(other <- handle(xs)) yield fillTab(tabs) + x.b.toString + "\n" + other*/
          case (x : Application) :: xs => for(done <- handleApplication(env, x, funcs, tabs); other <- handle(xs)) yield done + newLineStr(xs.exists(!isErased(_))) + other
          case (x : Binding) :: xs =>
            handleBinding(env, x, funcs) match{
              case Some(err) => Left(err)
              case None => for(other <- handle(xs)) yield other
            }
          case (x : ValueRef) :: xs =>
            handleValueRef(env, x) match{
              case Left(err) => Left(err)
              case Right(ok) => for(other <- handle(xs)) yield fillTab(tabs) + ok + newLineStr(xs.exists(!isErased(_))) + other
            }

          /*case (x : Entry) :: xs =>
            for(e <- handleEntry(env, tabs, x, funcs);rest <- handle(xs)) yield e + rest*/

          case (x : Write) :: xs =>
            handleWrite(env, x, funcs) match{
              case Some(err) => Left(err)
              case None =>
                handle(xs)
            }
          case (x : Read) :: xs =>
            handleRead(env, x, funcs) match{
              case Some(err) => Left(err)
              case None =>
                handle(xs)
            }

          case (x : Conditional) :: xs =>
            for(e <- handleConditional(tabs, env, x, funcs);rest <- handle(xs)) yield e + rest

          case Nil => Right("")
          case x :: _ => Left(s"${x} is not implemented in conditional branch")
        }
      }

      val h = handle(body)

      envPop(env)

      h
    }

    def handleStringToBool(str : String) : Either[ErrString,Boolean] = {
      if(str == "true")
        Right(true)
      else if(str == "false")
        Right(false)
      else
        Left(s"Condition must be a boolean value: `${str}`")
    }

    def handleConditionalCond(env : BindingEnv, cond : Expr, funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString, Boolean] = {
      cond match{
        //case b : BoolLit => Right(b.b)
        case s : StringExpr =>
          handleStringExpr(env, 0, false, s, funcs) match{
            case Left(err) => Left(err)
            case Right(ok) => handleStringToBool(ok)
          }
        case r : ValueRef =>
          handleValueRef(env, r) match{
            case Left(err) => Left(err)
            case Right(ok) => handleStringToBool(ok)
          }
        case a : Application => handleApplication(env, a, funcs, tabs = 0) match{
          case Left(err) => Left(err)
          case Right(ok) =>
            handleStringToBool(ok)

        }
      }
    }

    def handleConditional(tabs : Int, env : BindingEnv, cond : Conditional, funcs : immutable.HashMap[String, FuncObj]) : Either[ErrString, String] = {

      val t = for(h <- handleConditionalCond(env, cond.cond, funcs)) yield {
        if(h){
          for(h <- handleConditionalBranch(tabs, env, cond.ifTrue, funcs)) yield h
        }else{
          for(h <- handleConditionalBranch(tabs, env, cond.ifFalse, funcs)) yield h
        }
      }

      t match{
        case Left(err) => Left(err)
        case Right(ok) => ok
      }
    }


    def handleWrite(env : BindingEnv, write : Write, funcs : immutable.HashMap[String, FuncObj]) : Option[ErrString] = {
      write.expr match{
        case expr : StringExpr =>
          handleStringExpr(env, 0, false, expr, funcs) match{
            case Left(err) => Some(err)
            case Right(ok) => out.println(Console.BLACK + ok + Console.RESET);None
          }
        case x => Option(s"Write is not implemented for $x")
      }
    }
    def handleRead(env : BindingEnv, read : Read,funcs : immutable.HashMap[String, FuncObj]) : Option[ErrString] = {
      val str = in.nextLine()
      handleBinding(env, Binding(read.name, StringExpr(str, Nil)), funcs)
    }




  }

}
