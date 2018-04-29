package me.russoul

import me.russoul.BuiltinFunctions.BuiltinFunc

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{NoPosition, Positional}




object CheckListParser extends RegexParsers {
  override def skipWhitespace: Boolean = true //will skip newlines !

  val specialSymbols = List("<-", "->", "//")
  val digit = "[0-9]"
  val stringForbids = "\\$\\#\n\\{\\}" //TODO move to special symbols

  val opSymbolsAsList = List("\\+", "\\-", "\\*", "\\=", "\\<", "\\>", "\\!", "\\&", "\\|", "\\/", "\\%")
  val opSymbols = "[" + opSymbolsAsList.reduce(_ + _) + "]"

  val symbolNameForbids = List("\\(", "\\)", ",", "\\s", "\"") //also contains `stringForbids`
  val applicationArgsForbids = List("\\(", "\\)", ",") //also contains `stringForbids`
  //TODO make forbids less confusing

  val reservedNames: List[String] = List("if", "else", "true", "false")

  //disallow names that can be converted to Double
  def extraSymNameCheck(name : String) : Boolean = { //TODO we can do better
    try{
      java.lang.Double.parseDouble(name)
      false
    }catch{
      case _ : NumberFormatException => true
    }
  }

  import CheckListAST._




  /*def parseApplicationArg : Parser[Expr] = {
    log(parseStringExpr(forbidExtraSymbols = applicationArgsForbids, allowInterpolators = true) | parseApplication | parseValueRef)("parseApplicationArg")
  }*/

  def parseApplicationArg : Parser[Expr] = {
    log(parseBinOperatorInsideInterpolator | parseUnOperatorInsideInterpolator | parseBoolLiteral | parseFloatingPointLiteral | parseApplicationInsideInterpolator | parseValueRefInsideInterpolator | parseQuotedString)("parseApplicationArg")
  }

  def parseApplicationInsideInterpolator : Parser[Application] = {
    log((cond[StringExpr](log(parseStringExpr(symbolNameForbids))("application:name"), x => !reservedNames.contains(x.str) && extraSymNameCheck(x.str), x => s"illegal use of symbol `${x.str}`", commit = true) <~ literal("(")) ~
      (repsep(parseTab ~> parseApplicationArg <~ parseTab, literal(",")) <~ literal(")")) ^? {
      case name ~ args if !(BuiltinFunctions.builtinFunc.exists(op => op.arity == 2 && op.name == name.str) && args.length == 1) => //should fail if bin op which has a corresponding un op is called with one arg
        Application(name.str, args)
    })("parseApplication")
  }

  def parseApplication : Parser[Application] = {
    regexNonSkip("\\$".r) ~> parseApplicationInsideInterpolator
  }

  /*def parseApplication : Parser[Application] = {
    log(regexNonSkip("\\$".r) ~> (log(cond[StringExpr](parseStringExpr(forbidExtraSymbols = symbolNameForbids), (x:StringExpr) => !reservedNames.contains(x.str) && extraSymNameCheck(x.str), (x:StringExpr) => s"illegal use of builtin symbol `${x.str}`", commit = true))("application:name") <~ literal("(")) ~
      (repsep(parseApplicationArg, literal(",")) <~ literal(")"))^^ {
      case name ~ args => Application(name.str, args)
    })("parseApplication")
  }*/


  def parseStringInterpolatorExpr : Parser[Expr] = {
     log(parseBinOperatorInsideInterpolator | parseUnOperatorInsideInterpolator | parseFloatingPointLiteral | parseBoolLiteral | parseApplicationInsideInterpolator | parseValueRefInsideInterpolator | parseQuotedString)("parseStringInterpolatorExpr")
  }




  def parseStringInterpolator : Parser[Expr] = {
    log(regexNonSkip("\\$\\{".r) ~> commit(parenthesised(parseStringInterpolatorExpr)) <~ regexNonSkip("\\}".r))("parseStringInterpolator") ^^ StringInterpolatorExpr
  }

  def parseValueRef : Parser[ValueRef] = {
    log(regexNonSkip("\\$".r) ~> parseValueRefInsideInterpolator)("parseValueRef")
  }

  def parseBindingExpr : Parser[Expr] = {
    log(parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef)("parseBindingExpr")
  }

  def parseBinding : Parser[Binding] = {
    log((( literal("$") ~> parseStringExpr(forbidExtraSymbols = "\\=" :: symbolNameForbids) ) <~ literal("=")) ~ parseBindingExpr ^^ {
      case name ~ expr => Binding(name.str, expr)
    })("parseBinding")
  }

  def parseWriteExpr : Parser[Expr] = {
    log(parseConditional | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef)("parseWriteExpr")
  }

  def parseWrite : Parser[Write] = {
    log((regexNonSkip("\\<\\-".r) ~> parseWriteExpr) ^^ Write)("parseWrite")
  }

  def parseRead : Parser[Read] = {
    log(regexNonSkip("\\-\\>".r) ~> (parseTab ~> parseValueRefInsideInterpolator) ^^ { x => Read(x.name) })("parseRead")

  }

  def parseFloatingPointLiteral : Parser[StringExpr] = {
    log(regexNonSkip("""(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r) ^^ {x => StringExpr(x, Nil)})("parseFloatingPointLiteral")
  }

  def parseBoolLiteral : Parser[StringExpr] = {
    log((regexNonSkip("true".r) | regexNonSkip("false".r)) ^^ {x => StringExpr(x, Nil)})("parseBoolLiteral")
  }

  def parseQuotedString : Parser[Expr] = {
    log(regexNonSkip("\"".r) ~> parseStringExpr(forbidExtraSymbols = List("\""), allowInterpolators = true) <~ regexNonSkip("\"".r) ^^{
      x =>
        println(s"parsed quoted string ${x}")
        x
    })("parseQuotedString")
  }

  def parseValueRefInsideInterpolator : Parser[ValueRef] = {
    log(cond(parseStringExpr(forbidExtraSymbols = symbolNameForbids ++ opSymbolsAsList), (x:StringExpr) => !reservedNames.contains(x.str) && !BuiltinFunctions.builtinFunc.exists(f => f.name == x.str) && extraSymNameCheck(x.str), (x:StringExpr) => s"illegal symbol name `${x.str}`", commit = true) ^^ { x => ValueRef(x.str)})("parseValueRefInsideInterpolator")
  }

  def parseOperatorSymbolInsideInterpolator : Parser[String] = {
    log(cond[String]( regexNonSkip((opSymbols+"+").r) , (x:String) => !reservedNames.contains(x), (x) => s"illegal use of symbol `${x}`", commit = true))("parseOperatorSymbolInsideInterpolator")
  }



  def parsePrefixUnaryOperatorInsideInterpolator : Parser[(BuiltinFuncObj, (Expr) => Application)] = {
    import BuiltinFunctions._
    log(cond(parseOperatorSymbolInsideInterpolator, (x:String) => builtinFunc.exists(op => op.arity == 1 && op.name == "unary_"+x), (x:String) => s"prefix unary operator `unary_${x}` not found", commit = false) ^^ {f  => (builtinFunc.find(op => op.arity == 1 && op.name == "unary_"+f).get, (a: Expr) => Application("unary_"+f, List(a))) })("parsePrefixUnaryOperatorInsideInterpolator")
  }

  def parseBinaryOperatorInsideInterpolator : Parser[(BuiltinFuncObj, (Expr,Expr) => Application)] = {
    import BuiltinFunctions._
    log(cond(parseOperatorSymbolInsideInterpolator, (x:String) => builtinFunc.exists(op => op.arity == 2 && op.name == x), (x:String) => s"binary operator `${x}` not found", commit = false) ^^ {f => (builtinFunc.find(op => op.arity == 2 && op.name == f).get, (a : Expr, b : Expr) => Application(f, List(a,b)))  })("parseBinaryOperatorInsideInterpolator")
  }




  def applyOperatorRules(args : Array[Expr], ops : Array[((Expr,Expr) => Application, BuiltinFuncObj)]) : Either[String, Application] = {

    println("preping to apply rules for: ")
    ops.foreach(x => println(x._2.name))


    var leastPrecedenceIndices : List[Int] = Nil
    var leastPrecedence : Int = Int.MaxValue

    val errStr = "precedence and associativity rules are not satisfied"

    for(i <- ops.indices){
      val op = ops(i)
      if(op._2.precedence < leastPrecedence){
        leastPrecedence = op._2.precedence
        leastPrecedenceIndices = List(i)
      }else if(op._2.precedence == leastPrecedence){
        leastPrecedenceIndices = i :: leastPrecedenceIndices
      }
    }

    val curOps = for(i <- leastPrecedenceIndices) yield ops(i)

    println("curOps=" + curOps)


    //a |1| b (|2| c)... is not valid if at least one |i| is non associative and they all have the same precedence level
    if(leastPrecedenceIndices.size > 1 && curOps.exists(_._2.assoc.contains(AssociativityNone)))
      return Left(errStr)


    //operators of the same precedence level and tree level must have the same associativity
    if(curOps.tail.foldLeft((false,curOps.head._2)){ case (unsatisfaction, op) => if(unsatisfaction._1 || op._2.assoc != unsatisfaction._2.assoc) (true, null) else unsatisfaction }._1)
      return Left(errStr)



    //last element in ops becomes first in leastPrecedence list

    def apply(list : List[Int]): Either[String, Application] ={
      for(i <- list){ //TODO check if we are going from the back to the front, reverse list otherwise
        val op = ops(i)


        val frontOps = ops.slice(0, i)
        val backOps = ops.slice(i + 1, ops.length)

        println(s"front=${frontOps.toList}")
        println(s"back=${backOps.toList}")

        if(frontOps.isEmpty){ //== (i = 0) ?
          val a = args(i)
          val b = if(backOps.isEmpty){
            args(i+1)
          }else{
            applyOperatorRules(args.slice(i+1,args.length), backOps) match{
              case Left(err) => return Left(err)
              case Right(ok) => ok
            }
          }

          println(s"a=${a}\nb=${b}")

          return Right(Application(op._2.name, List(a,b)))
        }else{
          val a = applyOperatorRules(args.slice(0, i+1), frontOps) match{
            case Left(err) => return Left(err)
            case Right(ok) => ok
          }
          val b = if(backOps.isEmpty){
            args(i+1)
          }else{
            applyOperatorRules(args.slice(i+1,args.length), backOps) match{
              case Left(err) => return Left(err)
              case Right(ok) => ok
            }
          }
          return Right(Application(op._2.name, List(a,b))) //TODO pos must be set in func obj when parsed
        }
      }

      throw new Exception("impossible happened")
    }

    curOps.head._2.assoc match{
      case Some(AssociativityLeft) =>
        apply(leastPrecedenceIndices)
      case Some(AssociativityRight) =>
        apply(leastPrecedenceIndices.reverse)
      case Some(AssociativityNone) =>
        //only one op
        apply(leastPrecedenceIndices)
      case x => throw new Exception(s"impossible happened on case : ${x}")

    }


  }


  def parseBinOperatorArgumentInsideInterpolator : Parser[Expr] = {
    log((parseFloatingPointLiteral | parseBoolLiteral | parseUnOperatorInsideInterpolator | parseApplicationInsideInterpolator | parseValueRefInsideInterpolator | parseQuotedString) |
      (regexNonSkip("\\(".r) ~> (parseBinOperatorInsideInterpolator | parseFloatingPointLiteral | parseBoolLiteral | parseUnOperatorInsideInterpolator | parseApplicationInsideInterpolator | parseValueRefInsideInterpolator | parseQuotedString) <~ regexNonSkip("\\)".r)))("parseBinOperatorArgumentInsideInterpolator")
  }

  def parseUnOperatorArgumentInsideInterpolator : Parser[Expr] = {
     log((parseFloatingPointLiteral | parseBoolLiteral | parseApplicationInsideInterpolator | parseValueRefInsideInterpolator | parseQuotedString) |
      (regexNonSkip("\\(".r) ~> (parseBinOperatorInsideInterpolator | parseUnOperatorInsideInterpolator | parseFloatingPointLiteral | parseBoolLiteral | parseApplicationInsideInterpolator | parseValueRefInsideInterpolator | parseQuotedString) <~ regexNonSkip("\\)".r)))("parseUnOperatorArgumentInsideInterpolator")
  }

  def parseUnOperatorInsideInterpolator : Parser[Application] = {
    log((parsePrefixUnaryOperatorInsideInterpolator <~ parseTab) ~ parseUnOperatorArgumentInsideInterpolator ^^ {case f ~ x => f._2(x)})("parseUnOperatorInsideInterpolator")
  }

  def parenthesised[T](p : Parser[T]) : Parser[T] = {
    p | (regexNonSkip("\\(".r) ~> parenthesised(p) <~ regexNonSkip("\\)".r))
  }

  def parseBinOperatorInsideInterpolator : Parser[Application] = {
    log(( parenthesised(parseBinOperatorArgumentInsideInterpolator) ~ rep1((parseTab ~> parseBinaryOperatorInsideInterpolator  <~ parseTab) ~ commit(parenthesised(parseBinOperatorArgumentInsideInterpolator)))  ^^ {
      case arg0 ~ xs =>
        //val direct = xs.foldLeft(arg0){case (x, f ~ y) => f._2(x,y)}
        //println("parsed operator : " + direct)
        val ruled = applyOperatorRules( (arg0 :: xs.map(x => x._2)).toArray, xs.map(x => (x._1._2,x._1._1) ).toArray )
        ruled
    }) >> {
      case Right(ok) => (in => Success(ok, in)) : Parser[Application]
      case Left(bad) => (in => Error(bad, in)) : Parser[Application]
    })("parseBinOperatorInsideInterpolator")

  }


  def parseStringExpr(forbidExtraSymbols : List[String] = Nil, allowInterpolators : Boolean = false) : Parser[StringExpr] = {

    log{
      val parser =
      if(!allowInterpolators)
        rep1(guard(not( specialSymbols.map(literal).reduce(_ | _)  | regexNonSkip("\n".r)) | regex("$".r)).withFailureMessage("use of reserved symbol as text") ~> regexNonSkip(s"[^$stringForbids${forbidExtraSymbols.foldLeft("")((x,y) => x + y)}]".r))
      else
        rep1(rep1(guard(not( specialSymbols.map(literal).reduce(_ | _) | regexNonSkip("\n".r) ) | regex("$".r)).withFailureMessage("use of reserved symbol as text") ~> regexNonSkip(s"[^$stringForbids${forbidExtraSymbols.foldLeft("")((x,y) => x + y)}]".r)) | parseStringInterpolator | parseApplication | parseValueRef)


      val res = parser ^? { case xs if xs.exists(x => x.isInstanceOf[String] || x.isInstanceOf[List[String]] || x.isInstanceOf[StringInterpolatorExpr]) => //at least one char or one interpolator is required
        var curIndex = 0
        var fullString = ""
        val interpols = new ListBuffer[StringInterpolator]
        for (x <- xs) {
          x match {
            case string: String =>
              curIndex += string.length
              fullString += string
            case listString : List[String] =>
              val string = listString.reduce(_ + _)
              curIndex += string.length
              fullString += string
            case int : StringInterpolatorExpr =>
              interpols += StringInterpolator(curIndex, int.e)
            case e : Expr =>
              interpols += StringInterpolator(curIndex, e)
          }
        }
        StringExpr(fullString, interpols.toList)

      }
      res
    }("parseStringExpr")
  }



  def regexNonSkip(r: Regex): Parser[String] = (in: Input) => {
    val source = in.source
    val offset = in.offset


    (r findPrefixMatchOf (new SubSequence(source, offset))) match {
      case Some(matched) =>
        Success(source.subSequence(offset, offset + matched.end).toString,
          in.drop(matched.end))
      case None =>
        val found = if (offset == source.length()) "end of source" else "`" + source.charAt(offset) + "'"
        Failure("string matching regex '" + r + "' expected but " + found + " found", in)
    }
  }

  //parses further and further but only while condition holds (breaks if not)
  def rep1Cond[T](first: => Parser[T], p0: => Parser[T], cond : List[T] => Boolean): Parser[List[T]] = Parser { in =>
    lazy val p = p0 // lazy argument
    val elems = new ListBuffer[T]

    def continue(in: Input): ParseResult[List[T]] = {
      val p0 = p    // avoid repeatedly re-evaluating by-name parser
      @tailrec def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
        case Success(x, rest) =>
          if (cond(elems.toList ++ List(x)/*pushing back because ListBuffer pushes back*/)){
            elems += x ; applyp(rest)
          }else{
            println("stopped at \n" + in0.pos.longString)
            Success(elems.toList, in0)
          }
        case e @ Error(_, _)  => e  // still have to propagate error
        case other                => Success(elems.toList, in0)
      }

      applyp(in)
    }

    first(in) match {
      case Success(x, rest) =>
        if(cond(List(x))){
          elems += x ; continue(rest)
        }else{
          println("stopped at \n" + in.pos.longString)
          Failure("cond failed at " + in.pos.longString, in)
        }
      case ns: NoSuccess    => ns
    }
  }

  def rep1Cond[T](p: => Parser[T], cond : List[T] => Boolean): Parser[List[T]] = rep1Cond(p, p, cond)

  def repCond[T](p: => Parser[T], cond : List[T] => Boolean, commit : Boolean): Parser[List[T]] = rep1Cond(p, cond) | success(List())

  def cond[T](p : => Parser[T], cond : T => Boolean, msg : T => String, commit : Boolean) : Parser[T] = {
    in =>
      val res = p(in)

      res match{
        case suc@Success(r,_) =>
          if(cond(r)){
            suc
          }else{
            if(!commit){
              Failure(msg(r), in)
            }else{
              Error(msg(r), in)
            }
          }
        case other => other
      }
  }


  def parseNewLine : Parser[Object] = {
    regexNonSkip("\n".r)
  }

  /*def parseIntLit : Parser[IntLit] = {
    opt("-") ~ rep1(digit.r) <~ guard(regex("\\s+\n".r) | regex("$".r)) ^^{ //`1` is int literal, `1)` is string literal
      case maybeMinus ~ digits => IntLit(Integer.parseInt(
        (maybeMinus match{
          case Some(_) => "-"
          case None => ""
        }) + digits.reduce(_ + _)
      ))
    }
  }*/

  /*def parseBoolLit : Parser[BoolLit] = {
    (literal("true") | literal("false")) ^^ {
      x => if(x == "true") BoolLit(true) else BoolLit(false)
    }
  }*/

  def parseTab : Parser[Int] = {
    rep(regexNonSkip("\\s".r)) ^^ {x =>
      x.length
    }
  }

  def skipEmptyLines : Parser[List[String]] = {
    rep(regexNonSkip("\\s*\n".r))
  }

  def parseTabAtLeast(count : Int): Parser[Int] = {
    parseTab ^? ({case x if x >= count => x},x => s"At least $count whitespace is required, $x found")
  }


  def parseFunctionBodyExpr : Parser[Expr] = {
    parseWrite | parseRead | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }


  def parseFunction : Parser[Function] = {
    log(parseTab >> (tab => (literal("$$") ~> commit(cond[StringExpr](parseStringExpr(forbidExtraSymbols = symbolNameForbids), x => !reservedNames.contains(x.str) && !BuiltinFunctions.builtinFunc.exists(sym => sym.name == x.str) && extraSymNameCheck(x.str), x => s"cannot use `${x.str}` as a function name", commit = true)) <~
      literal("(")) ~ (((repsep(parseStringExpr(forbidExtraSymbols = symbolNameForbids), literal(",")) <~ literal(")")) <~ parseNewLine) ~
      ( commit(skipEmptyLines ~> ((guard(parseTabAtLeast(tab+1)) ~> parseConditional) | (parseTabAtLeast(tab+1) ~> parseFunctionBodyExpr)) ) ~ rep((parseNewLine ~> skipEmptyLines) ~> ((guard(parseTabAtLeast(tab+1)) ~> parseConditional) | (parseTabAtLeast(tab+1) ~> parseFunctionBodyExpr) ) )))) ^^ {
      case name ~ (args ~ (first ~ rest)) =>
        val mergedBody = first :: rest
        Function(name.str, args.map(_.str), mergedBody)
    })("parseFunction")
  }


  def parseEntryBodyExpr : Parser[Expr] = {
    parseWrite | parseRead | parseBinding | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseEntry : Parser[Entry] = {
    log(parseTab >> (tab =>
      (regexNonSkip("\\#".r) ~> parseStringExpr(forbidExtraSymbols = Nil) <~ parseNewLine) ~
        (((skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseEntryBodyExpr) | guard(parseTabAtLeast(tab + 1)) ~> (parseEntry | parseConditional)) ~ rep(parseNewLine ~> skipEmptyLines ~> ((parseTabAtLeast(tab + 1) ~> parseEntryBodyExpr) | guard(parseTabAtLeast(tab + 1)) ~> (parseEntry | parseConditional))))

    ) ^^ {
      case name ~ (x ~ xs)=>
        Entry(name.str, x :: xs)
    })("parseEntry")
  }






  def parseConditionalBody : Parser[Expr] = {
    parseWrite | parseRead | parseBinding | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseConditional : Parser[Conditional] = {
    log(parseTab >> (tab =>
      ((literal("$if{") ~> parenthesised(parseStringInterpolatorExpr) <~ literal("}")) <~ parseNewLine) ~
        (( (guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseConditionalBody)) ~ rep(parseNewLine ~> skipEmptyLines ~> ((guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (parseTabAtLeast(tab + 1) ~> parseConditionalBody)))) ~ opt(parseNewLine ~> skipEmptyLines ~> parseElseBranch(tab))) ^^ {
      case cond ~ (x ~ xs) ~ optElse=>
        optElse match{
          case None => Conditional(cond, x :: xs, Nil)
          case Some(elsee) => Conditional(cond, x :: xs, elsee)
        }
    })("parseConditional")
  }

  def parseElseBranch(tabReq : Int) : Parser[List[Expr]] = {
    log(parseTab >> (tab =>
      cond((literal("$else") <~ parseNewLine) ~>
        (( (guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseConditionalBody)) ~ rep(parseNewLine ~> skipEmptyLines ~> ((guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (parseTabAtLeast(tab + 1) ~> parseConditionalBody)))) , (_ : ~[Expr, List[Expr]]) => tabReq == tab, (_ : ~[Expr, List[Expr]]) => s"if and else must have the same tabulation got if=${tabReq} else=${tab}", false)) ^^ {
      case (x ~ xs) =>
        x :: xs
    })("parseElseBranch")
  }


  def parseCheckListBodyExpr : Parser[Expr] = {
    parseWrite | parseRead | parseConditional | parseFunction | parseEntry | parseBinding | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseCheckList : Parser[CheckList] = {
    (regexNonSkip("\\#\\#".r) ~> parseStringExpr(Nil) <~ parseNewLine) ~
      (skipEmptyLines ~> commit(parseCheckListBodyExpr) ~ rep(parseNewLine ~> skipEmptyLines ~> commit(parseCheckListBodyExpr))) ^^ {
      case name ~ (x ~ xs) =>
        CheckList(name.str, x :: xs)
    }
  }

  def parseFully[T](p : Parser[T]) : Parser[T] = {
    in =>
      p(in) match{
        case Failure(msg, next) => Error(msg,next)
        case Error(msg, next) => Error(msg,next)
        case Success(res, next) =>
          if(next.atEnd){
            Success(res,next)
          }else{
            Error("End of stream expected", next)
          }
      }
  }


  final class NoImplicit[A]

  object NoImplicit {
    implicit def noImplicit0[A]: NoImplicit[A] = new NoImplicit[A]
    implicit def noImplicit1[A](implicit ev: A): NoImplicit[A] = new NoImplicit[A]
  }

  trait IsPositional[T]{
    val bool : Boolean
  }

  implicit def exprIsPositional[T](implicit ev : T  <:< Expr) : IsPositional[T] {
  } = new IsPositional[T] {
    override val bool: Boolean = true
  }

  implicit def notPositional[T](implicit no : NoImplicit[T <:< Expr]) : IsPositional[T] = {
    new IsPositional[T] {
      override val bool: Boolean = false
    }
  }



  override def positioned[T <: Positional](p: => Parser[T]): Parser[T] = Parser { in =>
    p(in) match {
      case Success(t, in1) => Success(if (t.pos == NoPosition) t setPos in.pos else t, in1)
      case ns: NoSuccess => ns
    }
  }


  def log[T : IsPositional](p: => Parser[T])(name: String): Parser[T] = Parser{ in =>
    println("trying "+ name +" at \n"+ in.pos.longString)
    val pos : Parser[T] = if(implicitly[IsPositional[T]].bool) positioned(p.asInstanceOf[Parser[T with Positional]]) else p
    val r = pos(in)
    println(name +" --> "+ r)
    r
  }

  def removeComments(string : String) : String = {
    string.replaceAll("\\/\\/.*", "")
  }


}
