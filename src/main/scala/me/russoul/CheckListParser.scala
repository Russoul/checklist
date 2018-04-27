package me.russoul

import me.russoul.BuiltinFunctions.BuiltinFunc

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional




object CheckListParser extends RegexParsers {
  override def skipWhitespace: Boolean = true //will skip newlines !

  val specialSymbols = List("<-", "->", "//")
  val digit = "[0-9]"
  val stringForbids = "\\$\\#\n\\{\\}" //TODO move to special symbols

  val symbolNameForbids = List("\\(", "\\)", ",", "\\s") //also contains `stringForbids`
  val applicationArgsForbids = List("\\(", "\\)", ",") //also contains `stringForbids`
  //TODO make forbids less confusing

  val reservedNames: List[String] = List("if", "else")


 import CheckListAST._


  def parseApplicationArg : Parser[Expr] = {
    parseStringExpr(forbidExtraSymbols = applicationArgsForbids, allowInterpolators = true) | parseApplication | parseValueRef
  }

  def parseApplicationInsideInterpolator(forbidQuote : Boolean) : Parser[Application] = {
    (cond[StringExpr](parseStringExpr(forbidExtraSymbols = if(!forbidQuote)symbolNameForbids else "\"" :: symbolNameForbids), x => !reservedNames.contains(x.str), x => s"illegal use of builtin symbol `${x.str}`", commit = true) <~ literal("(")) ~
      (repsep(parseApplicationArg, literal(",")) <~ literal(")")) ^^ {
      case name ~ args => Application(name.str, args)
    }
  }

  def parseApplication : Parser[Application] = {
    regexNonSkip("\\$".r) ~> parseApplicationInsideInterpolator(forbidQuote = false)
  }


  def parseStringInterpolatorExpr : Parser[Expr] = {
     parseOperatorInsideInterpolator | parseApplicationInsideInterpolator(forbidQuote = true) | parseValueRefInsideInterpolator(forbidQuoteAndParentheses = true) | parseQuotedString
  }


  def parseStringInterpolator : Parser[Expr] = {
    regexNonSkip("\\$\\{".r) ~> ((regexNonSkip("\\(".r) ~> parseStringInterpolatorExpr <~ regexNonSkip("\\)".r)) | parseStringInterpolatorExpr) <~ regexNonSkip("\\}".r) ^^ StringInterpolatorExpr
  }

  def parseValueRef : Parser[ValueRef] = {
    regexNonSkip("\\$".r) ~> parseValueRefInsideInterpolator(forbidQuoteAndParentheses = false)
  }

  def parseBindingExpr : Parser[Expr] = {
    parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseBinding : Parser[Binding] = {
    (( literal("$") ~> parseStringExpr(forbidExtraSymbols = "\\=" :: symbolNameForbids) ) <~ literal("=")) ~ parseBindingExpr ^^ {
      case name ~ expr => Binding(name.str, expr)
    }
  }

  def parseWriteExpr : Parser[Expr] = {
    parseConditional | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseWrite : Parser[Write] = {
    (regexNonSkip("\\<\\-".r) ~> parseWriteExpr) ^^ Write
  }

  def parseRead : Parser[Read] = {
    (regexNonSkip("\\-\\>".r) ~> parseStringExpr(forbidExtraSymbols = symbolNameForbids)) ^^ { x => Read(x.str)}

  }

  def parseCommentLine : Parser[Expr] = {
    (regexNonSkip("\\/\\/".r) ~> regexNonSkip("[^\n]*".r)) ^^ CommentLine
  }


  def parseQuotedString : Parser[Expr] = {
    regexNonSkip("\"".r) ~> parseStringExpr(forbidExtraSymbols = List("\""), allowInterpolators = true) <~ regexNonSkip("\"".r) ^^{
      x =>
        println(s"parsed quoted string ${x}")
        x
    }
  }

  def parseValueRefInsideInterpolator(forbidQuoteAndParentheses : Boolean) : Parser[ValueRef] = {
    cond(parseStringExpr(forbidExtraSymbols = if (!forbidQuoteAndParentheses) symbolNameForbids else "\\(" :: "\\)" :: "\"" :: symbolNameForbids), (x:StringExpr) => !reservedNames.contains(x.str) && !BuiltinFunctions.builtinFunc.exists(f => f.name == x.str), (x:StringExpr) => s"illegal use of builtin symbol `${x.str}`", commit = true) ^^ { x => ValueRef(x.str)}
  }

  def parseOperatorSymbolInsideInterpolator : Parser[String] = {
    cond(parseStringExpr(forbidExtraSymbols = "\"" :: symbolNameForbids), (x:StringExpr) => !reservedNames.contains(x.str), (x:StringExpr) => s"illegal use of builtin symbol `${x.str}`", commit = true) ^^ {x => x.str}
  }



  def parsePrefixUnaryOperatorInsideInterpolator : Parser[Application] = {
    import BuiltinFunctions._
    (cond(parseOperatorSymbolInsideInterpolator, (x:String) => builtinFunc.exists(op => op.arity == 1 && op.name == x), (x:String) => s"prefix unary operator `${x}` not found", commit = true) ~ parseOperatorArgumentInsideInterpolator) ^^ {case f ~ a => Application(f, List(a))}
  }

  def parseBinaryOperatorInsideInterpolator : Parser[(Expr,Expr) => Application] = {
    import BuiltinFunctions._
    cond(parseOperatorSymbolInsideInterpolator, (x:String) => builtinFunc.exists(op => op.arity == 2 && op.name == x), (x:String) => s"binary operator `${x}` not found", commit = true) ^^ {f => (a : Expr, b : Expr) => Application(f, List(a,b))}
  }

  def chainl2[T, U](first: => Parser[T], p: => Parser[U], q: => Parser[(T, U) => T]): Parser[T]
     = first ~ rep1(q ~ p) ^^ {
    case x ~ xs => xs.foldLeft(x: T){case (a, f ~ b) => f(a, b)} // x's type annotation is needed to deal with changed type inference due to SI-5189
  }

  def parseOperatorArgumentInsideInterpolator : Parser[Expr] = {
    (regexNonSkip("\\(".r) ~> (parseOperatorInsideInterpolator ^^ BoundApplication) <~ regexNonSkip("\\)".r)) | (parseApplicationInsideInterpolator(forbidQuote = true) | parseValueRefInsideInterpolator(forbidQuoteAndParentheses = true) | parseQuotedString) |
      (regexNonSkip("\\(".r) ~> (parseApplicationInsideInterpolator(forbidQuote = true) | parseValueRefInsideInterpolator(forbidQuoteAndParentheses = true) | parseQuotedString) <~ regexNonSkip("\\)".r))
  }


  def applyOperatorRules(app : Application) : Application = { //TODO dont forget to get rid of bound applications(map them to regular ones)

    def byOperator(application: Application) : List[Application] = {

      def byOperatorList(list : List[Expr]) : List[Application] = {
        list match{
          case (x : Application) :: xs => x :: byOperatorList(xs)
          case _ :: xs => byOperatorList(xs)
          case Nil => Nil
        }
      }

      application match{
        case app@Application(_, args) =>
          app :: byOperatorList(args)
      }
    }


    val ops = byOperator(app)

    val leastPrecedence = ops.foldLeft( (Nil : List[(Application, BuiltinFuncObj)],Int.MaxValue) ) ( (xs : (List[(Application, BuiltinFuncObj)],Int), x) => {
      val op = BuiltinFunctions.builtinFunc.find(suchOp => suchOp.arity == 2 && suchOp.name == x.name).get //guaranteed as the parser has parsed(found) the function by that line
      if(op.precedence < xs._2){
        ((x,op) :: Nil, op.precedence)
      }else if(op.precedence == xs._2){
        ((x,op) :: xs._1, xs._2)
      }

    })._1

    for(op <- leastPrecedence){
      op._2.assoc match{
        case AssociativityNone =>
          Application(op._1.name, List())
      }
    }
  }




  def parseOperatorInsideInterpolator : Parser[Application] = {
    chainl2[Expr,Expr](parseOperatorArgumentInsideInterpolator, parseOperatorArgumentInsideInterpolator, parseTab ~> parseBinaryOperatorInsideInterpolator <~ parseTab) ^^ {x =>
      println("parsed operator : " + x)
      //TODO precedence + unary
      x.asInstanceOf[Application]
    }

  }


  def parseStringExpr(forbidExtraSymbols : List[String] = Nil, allowInterpolators : Boolean = false) : Parser[StringExpr] = {

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
    parseWrite | parseRead | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }


  def parseFunction : Parser[Function] = {
    parseTab >> (tab => (literal("$$") ~> commit(cond[StringExpr](parseStringExpr(forbidExtraSymbols = symbolNameForbids), x => !reservedNames.contains(x.str) && !BuiltinFunctions.builtinFunc.contains(x.str), x => s"cannot use `${x.str}` as a function name", commit = true)) <~
      literal("(")) ~ (((repsep(parseStringExpr(forbidExtraSymbols = symbolNameForbids), literal(",")) <~ literal(")")) <~ parseNewLine) ~
      ( commit(skipEmptyLines ~> ((guard(parseTabAtLeast(tab+1)) ~> parseConditional) | (parseTabAtLeast(tab+1) ~> parseFunctionBodyExpr)) ) ~ rep((parseNewLine ~> skipEmptyLines) ~> ((guard(parseTabAtLeast(tab+1)) ~> parseConditional) | (parseTabAtLeast(tab+1) ~> parseFunctionBodyExpr) ) )))) ^?({
      case name ~ (args ~ (first ~ rest)) if (first :: rest).exists(x => !x.isInstanceOf[CommentLine]) =>
        val mergedBody = first :: rest
        Function(name.str, args.map(_.str), mergedBody)
    }, _ => "Function must contain at least one non comment expression")
  }


  def parseEntryBodyExpr : Parser[Expr] = {
    parseWrite | parseRead | parseConditional | parseBinding | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }

  def parseEntry : Parser[Entry] = {
    parseTab >> (tab =>
      (regexNonSkip("\\#".r) ~> parseStringExpr(forbidExtraSymbols = Nil) <~ parseNewLine) ~
        (((skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseEntryBodyExpr) | guard(parseTabAtLeast(tab + 1)) ~> parseEntry) ~ rep(parseNewLine ~> skipEmptyLines ~> ((parseTabAtLeast(tab + 1) ~> parseEntryBodyExpr) | guard(parseTabAtLeast(tab + 1)) ~> parseEntry)))

    ) ^? ({
      case name ~ (x ~ xs) if (x :: xs).exists(x => !x.isInstanceOf[CommentLine]) =>
        Entry(name.str, x :: xs)
    }, _ => "entry must contain at least one non comment expression")
  }




  def parseConditionalCond : Parser[Expr] = {
    parseStringExpr(Nil, allowInterpolators = true) | parseApplication | parseValueRef
  }

  def parseConditionalBody : Parser[Expr] = {
    parseWrite | parseRead | parseBinding | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }

  def parseConditional : Parser[Conditional] = {
    parseTab >> (tab =>
      ((literal("$if{") ~> parseConditionalCond <~ literal("}")) <~ parseNewLine) ~
        (( (guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseConditionalBody)) ~ rep(parseNewLine ~> skipEmptyLines ~> ((guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (parseTabAtLeast(tab + 1) ~> parseConditionalBody)))) ~ opt(parseNewLine ~> skipEmptyLines ~> parseElseBranch(tab))) ^? ({
      case cond ~ (x ~ xs) ~ optElse if (x :: xs).exists(x => !x.isInstanceOf[CommentLine]) =>
        optElse match{
          case None => Conditional(cond, x :: xs, Nil)
          case Some(elsee) => Conditional(cond, x :: xs, elsee)
        }
    }, _ => "if branch must contain at least one non comment expression")
  }

  def parseElseBranch(tabReq : Int) : Parser[List[Expr]] = {
    parseTab >> (tab =>
      cond((literal("$else") <~ parseNewLine) ~>
        (( (guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseConditionalBody)) ~ rep(parseNewLine ~> skipEmptyLines ~> ((guard(parseTabAtLeast(tab + 1)) ~> parseConditional) | (parseTabAtLeast(tab + 1) ~> parseConditionalBody)))) , (_ : ~[Expr, List[Expr]]) => tabReq == tab, (_ : ~[Expr, List[Expr]]) => "if and else must have the same tabulation", false)) ^? ({
      case (x ~ xs) if (x :: xs).exists(x => !x.isInstanceOf[CommentLine]) =>
        x :: xs
    }, _ => "else branch must contain at least one non comment expression")
  }


  def parseCheckListBodyExpr : Parser[Expr] = {
    parseWrite | parseRead | parseConditional | parseFunction | parseEntry | parseBinding | parseStringExpr(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }

  def parseCheckList : Parser[CheckList] = {
    (regexNonSkip("\\#\\#".r) ~> parseStringExpr(Nil) <~ parseNewLine) ~
      (skipEmptyLines ~> parseCheckListBodyExpr ~ rep(parseNewLine ~> skipEmptyLines ~> parseCheckListBodyExpr)) ^^ {
      case name ~ (x ~ xs) =>
        CheckList(name.str, x :: xs)
    }
  }

  def parseFully(str : String) : ParseResult[CheckList] = {
    parseAll(parseCheckList,str)
  }

}
