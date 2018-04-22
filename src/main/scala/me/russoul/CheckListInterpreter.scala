package me.russoul

import CheckListAST._

import scala.collection.mutable
import scala.collection.immutable

object CheckListInterpreter {

  type Value = String //TODO remove this
  type ErrString = String
  final val TAB_SIZE = 3


  type BindingEnv = mutable.ListBuffer[mutable.HashMap[String, Value]]


  def envContains(env : BindingEnv, str : String) : Boolean = {
    if(env.isEmpty) throw new Exception("trying work with empty environment")
    for(hash <- env.reverse){ //TODO reverse not required here
      if(hash.contains(str)) return true
    }

    false
  }

  def envGet(env : BindingEnv, str : String) : Value = {
    if(env.isEmpty) throw new Exception("trying work with empty environment")
    for(hash <- env.reverse){
      hash.get(str) match{
        case Some(x) => return x
        case None => ()
      }
    }

    throw new Exception(s"environment does not contain ${str}")
  }

  def envGetOption(env : BindingEnv, str : String) : Option[Value] = {
    if(env.isEmpty) throw new Exception("trying work with empty environment")
    for(hash <- env.reverse){
      hash.get(str) match{
        case ok@Some(_) => return ok
        case None => ()
      }
    }

    None
  }

  def envPush(env : BindingEnv) : Unit = {
    env += new mutable.HashMap[String, Value]
  }

  def envPop(env : BindingEnv) : Unit = {
    if(env.isEmpty) throw new Exception("trying to pop empty env")
    env.remove(env.length - 1)
  }

  def envPut(env : BindingEnv, str : String, value : Value) : Unit = {
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

  def handleStringInterpolatorExprs(env : BindingEnv, list : List[Expr], funcs : immutable.HashMap[String, Function]) : Either[ErrString,List[Value]] = {
    list match{
      case (i : IntLit) :: xs => for(rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield i.i.toString :: rest
      case (b : BoolLit) :: xs => for(rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield b.b.toString :: rest
      case (s : StringLit) :: xs => for(str <- handleStringLit(env, 0, newLine = false, s, funcs); rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield str :: rest
      case (a : Application) :: xs => for(v <- handleApplication(env, a, funcs, newLine = false); rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield v :: rest
      case (r : ValueRef) :: xs => for(v <- handleValueRef(env, r); rest <- handleStringInterpolatorExprs(env, xs, funcs)) yield v :: rest
      case Nil => Right(Nil)
    }
  }

  def interpolateString(str : String, interpolation : List[(Int, Value)]) : String = {
    interpolation match{
      case x :: xs =>
        val newStr = str.substring(0, x._1) + x._2 + str.substring(x._1, str.length)
        interpolateString(newStr, xs.map{ case (i,s) =>  (i + x._2.length, s)})
      case Nil => str
    }
  }

  def handleStringLit(env : BindingEnv, tabs : Int, newLine : Boolean, lit : StringLit, funcs : immutable.HashMap[String, Function]) : Either[ErrString, Value] = {
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

  def handleEntry(env : BindingEnv, tabs : Int, entry : Entry, funcs : immutable.HashMap[String, Function]) : Either[ErrString, Value] = {
    val str = fillTab(tabs) + "#" + entry.name + "\n"

    envPush(env)

    def handle(list : List[Expr]) : Either[ErrString, Value] = {
      list match{
        case (x : StringLit) :: xs =>
          for(done <- handleStringLit(env, tabs + 1, newLine = true, x, funcs); other <- handle(xs)) yield done + other
        case (x : IntLit) :: xs =>
          for(other <- handle(xs)) yield fillTab(tabs + 1) + x.i.toString + "\n" + other
        case (x : BoolLit) :: xs =>
          for(other <- handle(xs)) yield fillTab(tabs + 1) + x.b.toString + "\n" + other
        case (x : Application) :: xs => for(done <- handleApplication(env, x, funcs, newLine = true); other <- handle(xs)) yield fillTab(tabs + 1) + done + other
        case (x : Binding) :: xs =>
          handleBinding(env, x, funcs) match{
            case Some(err) => Left(err)
            case None => for(other <- handle(xs)) yield other
          }
        case (x : ValueRef) :: xs =>
          handleValueRef(env, x) match{
            case Left(err) => Left(err)
            case Right(ok) => for(other <- handle(xs)) yield fillTab(tabs + 1) + ok + "\n" + other
          }
        case (x : Entry) :: xs =>
          for(e <- handleEntry(env, tabs + 1, x, funcs);rest <- handle(xs)) yield e + rest

        case Nil => Right("")
        case x :: _ => Left(s"${x} is not implemented in entry")
      }
    }

    val result = handle(entry.exprs)
    envPop(env)
    for(_result <- result) yield str + _result
  }

  def handleValueRef(env : BindingEnv, ref : ValueRef) : Either[ErrString, Value] = {
    envGetOption(env, ref.name) match{
      case None => Left(s"reference not found ${ref.name}")
      case Some(x) => Right(x)
    }
  }


  def newLineStr(newLine : Boolean): String = if(newLine) "\n" else ""

  def handleApplication(env : BindingEnv, apply: Application, funcs : immutable.HashMap[String, Function], newLine : Boolean) : Either[ErrString, Value] = {
    if(!funcs.contains(apply.name)){
      Left(s"function not found ${apply.name}")
    }else{
      val func = funcs(apply.name)

      handleApplicationArgs(env, apply.args, funcs) match{
        case Left(err) => Left(err)
        case Right(args) =>

          envPush(env)

          for((value, name) <- args.zip(func.args)){
            envPut(env, name, value)
          }

          def handle(exprs : List[Expr]) : Either[ErrString,Value] = {
            exprs match{
              case (x : StringLit) :: xs =>
                for(res <- handleStringLit(env, 0, newLine = newLine, x, funcs); other <- handle(xs)) yield res + other
              case (x : BoolLit) :: xs =>
                for(other <- handle(xs)) yield x.b.toString + newLineStr(newLine) + other
              case (x : IntLit) :: xs =>
                for(other <- handle(xs)) yield x.i.toString + newLineStr(newLine) + other
              case (x : ValueRef) :: xs =>
                for(ref <- handleValueRef(env, x); other <- handle(xs)) yield ref + newLineStr(newLine) + other
              case (x : Application) :: xs =>
                for(app <- handleApplication(env, x, funcs, newLine); other <- handle(xs)) yield app + other
              case Nil => Right("")
              case x :: _ => Left(s"Unsupported element in function: ${x}")
            }
          }

          val result = handle(func.body)
          envPop(env)
          result

      }



    }
  }


  def handleApplicationArgs(env : BindingEnv, args : List[Expr], funcs : immutable.HashMap[String, Function]) : Either[ErrString, List[Value]] = {
    args match{
      case (x : IntLit) :: xs => for(rest <- handleApplicationArgs(env, xs, funcs)) yield x.i.toString :: rest
      case (x : StringLit) :: xs => for(str <- handleStringLit(env, 0, newLine = false, x, funcs);rest <- handleApplicationArgs(env, xs, funcs)) yield str :: rest
      case (x : Application) :: xs => for(app <- handleApplication(env, x, funcs, newLine = false); rest <- handleApplicationArgs(env, xs, funcs)) yield app :: rest
      case (x : ValueRef) :: xs => for(value <- handleValueRef(env, x); rest <- handleApplicationArgs(env, xs, funcs)) yield value :: rest
      case Nil => Right(Nil)
      case x :: _ => throw new Exception(s"cannot handle ${x}")
    }
  }

  def handleBinding(env : BindingEnv, binding : Binding, funcs : immutable.HashMap[String, Function]) : Option[ErrString] = {
    binding.expr match{
      case x : IntLit => envPut(env, binding.name, x.i.toString); None
      case x : BoolLit => envPut(env, binding.name, x.b.toString); None
      case x : StringLit => handleStringLit(env, 0, newLine = false, x, funcs) match{
        case Left(err) => Some(err)
        case Right(str) => envPut(env, binding.name, str); None
      }
      case x : ValueRef =>
        handleValueRef(env, x) match{
          case Left(err) => Option(err)
          case Right(value) => envPut(env, binding.name, value); None
        }
      case x : Application =>
        handleApplication(env, x, funcs, newLine = false) match{
          case Left(err) => Option(err)
          case Right(ok) => envPut(env, binding.name, ok); None
        }
    }
  }

  def handleConditionalBranch(tabs : Int, env: BindingEnv, body : List[Expr], funcs : immutable.HashMap[String, Function]) : Either[ErrString, Value] = {
    envPush(env)

    def handle(list : List[Expr]) : Either[ErrString, Value] = {
      list match{
        case (x : StringLit) :: xs =>
          for(done <- handleStringLit(env, tabs, newLine = true, x, funcs); other <- handle(xs)) yield done + other
        case (x : IntLit) :: xs =>
          for(other <- handle(xs)) yield fillTab(tabs) + x.i.toString + "\n" + other
        case (x : BoolLit) :: xs =>
          for(other <- handle(xs)) yield fillTab(tabs) + x.b.toString + "\n" + other
        case (x : Application) :: xs => for(done <- handleApplication(env, x, funcs, newLine = true); other <- handle(xs)) yield fillTab(tabs) + done + other
        case (x : Binding) :: xs =>
          handleBinding(env, x, funcs) match{
            case Some(err) => Left(err)
            case None => for(other <- handle(xs)) yield other
          }
        case (x : ValueRef) :: xs =>
          handleValueRef(env, x) match{
            case Left(err) => Left(err)
            case Right(ok) => for(other <- handle(xs)) yield fillTab(tabs) + ok + "\n" + other
          }
        case (x : Entry) :: xs =>
          for(e <- handleEntry(env, tabs, x, funcs);rest <- handle(xs)) yield e + rest

        case (x : Conditional) :: xs =>
          for(e <- handleConditional(tabs, env, x, funcs);rest <- handle(xs)) yield e + rest

        case Nil => Right("")
        case x :: _ => Left(s"${x} is not implemented in entry")
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

  def handleConditionalCond(env : BindingEnv, cond : Expr, funcs : immutable.HashMap[String, Function]) : Either[ErrString, Boolean] = {
    cond match{
      case b : BoolLit => Right(b.b)
      case r : ValueRef =>
        handleValueRef(env, r) match{
          case Left(err) => Left(err)
          case Right(ok) => handleStringToBool(ok)
        }
      case a : Application => handleApplication(env, a, funcs, newLine = false) match{
        case Left(err) => Left(err)
        case Right(ok) =>
          handleStringToBool(ok)
      }
    }
  }

  def handleConditional(tabs : Int, env : BindingEnv, cond : Conditional, funcs : immutable.HashMap[String, Function]) : Either[ErrString, Value] = {
    for(h <- handleConditionalCond(env, cond.cond, funcs)) yield {
      if(h){
        handleConditionalBranch(tabs, env, cond.ifTrue, funcs) match{
          case Left(err) => err
          case Right(ok) => ok
        }
      }else{
        handleConditionalBranch(tabs, env, cond.ifFalse, funcs) match{
          case Left(err) => err
          case Right(ok) => ok
        }
      }
    }
  }


  def interpret(checklist : CheckList): Either[ErrString, String] ={
    val funcs = findFunctions(checklist)

    if(funcs.distinct.length != funcs.length){
      return Left("duplicate function names found !")
    }

    val funcHashmap = immutable.HashMap[String, Function](funcs.map(x => (x.name, x)) : _*)

    val bindingEnv = new mutable.ListBuffer[mutable.HashMap[String, Value]]
    envPush(bindingEnv)


    var str = "##" + checklist.name + "\n"

    for(expr <- checklist.exprs){
      expr match{
        case _ : Function => () //do nothing
        case expr : Binding =>
          handleBinding(bindingEnv, expr, funcHashmap) match{
            case Some(err) => return Left(err)
            case None => ()
          }
        case expr : Application =>
          handleApplication(bindingEnv, expr, funcHashmap, newLine = true) match{
            case err@Left(_) => return err
            case Right(ok) => str += ok
          }
        case expr : StringLit =>
          handleStringLit(bindingEnv, 0, newLine = true, expr, funcHashmap) match{
            case err@Left(_) => return err
            case Right(ok) => str += ok
          }
        case expr : Entry =>
          handleEntry(bindingEnv, 0, expr, funcHashmap) match {
            case err@Left(_) => return err
            case Right(ok) => str += ok
          }
        case expr : ValueRef =>
          handleValueRef(bindingEnv, expr) match{
            case err@Left(_) => return err
            case Right(ok) => str += ok
          }
        case expr : Conditional =>
          handleConditional(0, bindingEnv, expr, funcHashmap) match{
            case err@Left(_) => return err
            case Right(ok) => str += ok
          }
        case _ => return Left(s"not yet implemented ${expr} in CheckList body")
      }
    }

    Right(str)

  }

}
