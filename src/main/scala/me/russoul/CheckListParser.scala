package me.russoul

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional


object CheckListAST{
  case class StringInterpolator(index : Int, expr: Expr)


  sealed trait Expr extends Positional
  //case class IntLit(i : Int) extends Lit
  //case class BoolLit(b : Boolean) extends Lit
  case class StringLit(str : String, interpolators : List[StringInterpolator]) extends Expr //TODO rename class as it is not actually a literal
  case class ValueRef(name : String) extends Expr

  case class Conditional(cond : Expr, ifTrue : List[Expr], ifFalse : List[Expr]) extends Expr
  case class CheckList(name : String, exprs : List[Expr]) extends Expr
  case class Function(name : String, args : List[String], body : List[Expr]) extends Expr
  case class Binding(name : String, expr : Expr) extends Expr
  case class Application(name : String, args : List[Expr]) extends Expr

  case class Read(name : String) extends Expr
  case class Write(expr : Expr) extends Expr
  case class Entry(name : String, exprs : List[Expr]) extends Expr
  case class CommentLine(comment : String) extends Expr
}

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
    parseStringLit(forbidExtraSymbols = applicationArgsForbids, allowInterpolators = true) | parseApplication | parseValueRef
  }

  def parseApplication : Parser[Application] = {
    (regexNonSkip("\\$".r) ~> cond[StringLit](parseStringLit(forbidExtraSymbols = symbolNameForbids), x => !reservedNames.contains(x.str), x => s"illegal use of builtin symbol `${x.str}`", commit = true) <~ literal("(")) ~
      (repsep(parseApplicationArg, literal(",")) <~ literal(")")) ^^ {
        case name ~ args => Application(name.str, args)
      }
  }

  def parseStringInterpolatorExpr : Parser[Expr] = {
    parseApplication | parseValueRef | parseStringLit(forbidExtraSymbols = Nil, allowInterpolators = true)
  }

  case class StringInterpolatorExpr(e : Expr) extends Expr //used for disambiguation

  def parseStringInterpolator : Parser[Expr] = {
    literal("${") ~> parseStringInterpolatorExpr <~ literal("}") ^^ StringInterpolatorExpr
  }

  def parseValueRef : Parser[ValueRef] = {
    regexNonSkip("\\$".r) ~> cond(parseStringLit(forbidExtraSymbols = symbolNameForbids), (x:StringLit) => !reservedNames.contains(x.str) && !BuiltinFunctions.builtinOp.keys.toList.contains(x.str), (x:StringLit) => s"illegal use of builtin symbol `${x.str}`", commit = true) ^^ { x => ValueRef(x.str)}
  }

  def parseBindingExpr : Parser[Expr] = {
    parseStringLit(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseBinding : Parser[Binding] = {
    (( literal("$") ~> parseStringLit(forbidExtraSymbols = "\\=" :: symbolNameForbids) ) <~ literal("=")) ~ parseBindingExpr ^^ {
      case name ~ expr => Binding(name.str, expr)
    }
  }

  def parseWriteExpr : Parser[Expr] = {
    parseConditional | parseStringLit(Nil, allowInterpolators = true)  | parseApplication | parseValueRef
  }

  def parseWrite : Parser[Write] = {
    (regexNonSkip("\\<\\-".r) ~> parseWriteExpr) ^^ Write
  }

  def parseRead : Parser[Read] = {
    (regexNonSkip("\\-\\>".r) ~> parseStringLit(forbidExtraSymbols = symbolNameForbids)) ^^ {x => Read(x.str)}

  }

  def parseCommentLine : Parser[Expr] = {
    (regexNonSkip("\\/\\/".r) ~> regexNonSkip("[^\n]*".r)) ^^ CommentLine
  }


  def parseStringLit(forbidExtraSymbols : List[String] = Nil, allowInterpolators : Boolean = false) : Parser[StringLit] = {

    val parser =
      if(!allowInterpolators)
        rep1(guard(not( specialSymbols.map(literal).reduce(_ | _) ) | regexNonSkip("\n".r) | regex("$".r)).withFailureMessage("use of reserved symbol as text") ~> regexNonSkip(s"[^$stringForbids${forbidExtraSymbols.foldLeft("")((x,y) => x + y)}]".r))
      else
        rep1(rep1(guard(not( specialSymbols.map(literal).reduce(_ | _) )  | regexNonSkip("\n".r) | regex("$".r)).withFailureMessage("use of reserved symbol as text") ~> regexNonSkip(s"[^$stringForbids${forbidExtraSymbols.foldLeft("")((x,y) => x + y)}]".r))| parseStringInterpolator | parseApplication | parseValueRef)


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

      StringLit(fullString, interpols.toList)

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

  //parses futher and futher but only while condition holds (breaks if not)
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
        case suc@Success(r,n) =>
          if(cond(r)){
            suc
          }else{
            if(!commit){
              Failure(msg(r), n)
            }else{
              Error(msg(r), n)
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
    parseWrite | parseRead | parseStringLit(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }


  def parseFunction : Parser[Function] = {
    parseTab >> (tab => (literal("$$") ~> commit(cond[StringLit](parseStringLit(forbidExtraSymbols = symbolNameForbids), x => !reservedNames.contains(x.str) && !BuiltinFunctions.builtinOp.contains(x.str), x => s"cannot use `${x.str}` as a function name", commit = true)) <~
      literal("(")) ~ (((repsep(parseStringLit(forbidExtraSymbols = symbolNameForbids), literal(",")) <~ literal(")")) <~ parseNewLine) ~
      ( commit(skipEmptyLines ~> ((guard(parseTabAtLeast(tab+1)) ~> parseConditional) | (parseTabAtLeast(tab+1) ~> parseFunctionBodyExpr)) ) ~ rep((parseNewLine ~> skipEmptyLines) ~> ((guard(parseTabAtLeast(tab+1)) ~> parseConditional) | (parseTabAtLeast(tab+1) ~> parseFunctionBodyExpr) ) )))) ^?({
      case name ~ (args ~ (first ~ rest)) if (first :: rest).exists(x => !x.isInstanceOf[CommentLine]) =>
        val mergedBody = first :: rest
        Function(name.str, args.map(_.str), mergedBody)
    }, _ => "Function must contain at least one non comment expression")
  }


  def parseEntryBodyExpr : Parser[Expr] = {
    parseWrite | parseRead | parseConditional | parseBinding | parseStringLit(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }

  def parseEntry : Parser[Entry] = {
    parseTab >> (tab =>
      (regexNonSkip("\\#".r) ~> parseStringLit(forbidExtraSymbols = Nil) <~ parseNewLine) ~
        (((skipEmptyLines ~> commit(parseTabAtLeast(tab + 1)) ~> parseEntryBodyExpr) | guard(parseTabAtLeast(tab + 1)) ~> parseEntry) ~ rep(parseNewLine ~> skipEmptyLines ~> ((parseTabAtLeast(tab + 1) ~> parseEntryBodyExpr) | guard(parseTabAtLeast(tab + 1)) ~> parseEntry)))

    ) ^? ({
      case name ~ (x ~ xs) if (x :: xs).exists(x => !x.isInstanceOf[CommentLine]) =>
        Entry(name.str, x :: xs)
    }, _ => "entry must contain at least one non comment expression")
  }




  def parseConditionalCond : Parser[Expr] = {
    parseStringLit(Nil, allowInterpolators = true) | parseApplication | parseValueRef
  }

  def parseConditionalBody : Parser[Expr] = {
    parseWrite | parseRead | parseBinding | parseStringLit(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
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
    parseWrite | parseRead | parseConditional | parseFunction | parseEntry | parseBinding | parseStringLit(Nil, allowInterpolators = true)  | parseApplication | parseValueRef | parseCommentLine
  }

  def parseCheckList : Parser[CheckList] = {
    (regexNonSkip("\\#\\#".r) ~> parseStringLit(Nil) <~ parseNewLine) ~
      (skipEmptyLines ~> parseCheckListBodyExpr ~ rep(parseNewLine ~> skipEmptyLines ~> parseCheckListBodyExpr)) ^^ {
      case name ~ (x ~ xs) =>
        CheckList(name.str, x :: xs)
    }
  }

  def parseFully(str : String) : ParseResult[CheckList] = {
    parseAll(parseCheckList,str)
  }

}
