package me.russoul

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object CheckListParser extends RegexParsers {
  override def skipWhitespace: Boolean = true

  val specialSymbols = List('$', '#')
  val digit = "[0-9]"
  val stringForbids = "\\$\\#\n\\{\\}"

  val symbolNameForbids = List("\\(", "\\)", "\\s") //also contains `stringForbids`


  case class StringInterpolator(index : Int, expr: Expr)


  sealed trait Expr
  sealed trait Lit extends Expr
  sealed trait Type extends Expr
  object IntType extends Type
  object StringType extends Type
  case class IntLit(i : Int) extends Lit
  case class StringLit(str : String, interpolators : List[StringInterpolator]) extends Lit //TODO string interpolators

  case class CheckList(name : String, exprs : List[Expr]) extends Expr
  case class Function(name : String, args : List[Expr], body : List[Expr]) extends Expr
  case class Binding(name : String, expr : Expr) extends Expr
  case class Application(name : String, args : List[Expr]) extends Expr

  case class Read(name : String, typee : Type, lit : Lit) extends Expr
  case class Entry(name : String, exprs : List[Expr]) extends Expr


  def parseApplication : Parser[Application] = {
    (literal("$") ~> parseStringLit(forbidExtraSymbols = symbolNameForbids) <~ literal("(")) ~
      (repsep(parseIntLit | parseStringLit(forbidExtraSymbols = List(","), allowInterpolators = true), literal(",")) <~ literal(")")) ^^ {
        case name ~ args => Application(name.str, args)
      }
  }

  def parseStringInterpolatorExpr : Parser[Expr] = {
    parseIntLit | parseStringLit(forbidExtraSymbols = Nil, allowInterpolators = true) | parseApplication
  }

  def parseStringInterpolator : Parser[Expr] = {
    literal("${") ~> parseStringInterpolatorExpr <~ literal("}")
  }

  implicit def regexNonSkip(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = offset
      (r findPrefixMatchOf (new SubSequence(source, start))) match {
        case Some(matched) =>
          Success(source.subSequence(start, start + matched.end).toString,
            in.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  def parseStringLit(forbidExtraSymbols : List[String] = Nil, allowInterpolators : Boolean = false) : Parser[StringLit] = {
    val res = rep1(regexNonSkip(s"[^$stringForbids${forbidExtraSymbols.foldLeft("")((x,y) => x + y)}]+".r) | parseStringInterpolator) ^^ { xs =>
      var curIndex = 0
      var fullString = ""
      val interpols = new ListBuffer[StringInterpolator]
      for (x <- xs) {
        x match {
          case string: String =>
            curIndex += string.length
            fullString += string
          case interpol : Expr =>
            interpols += StringInterpolator(curIndex, interpol)
        }
      }

      StringLit(fullString, interpols.toList)

    }
    res
  }

  def parseNewLine : Parser[Object] = {
    regex("\n".r)
  }

  def parseIntLit : Parser[IntLit] = {
    opt("-") ~ rep1(digit.r) ^^{
      case maybeMinus ~ digits => IntLit(Integer.parseInt(
        (maybeMinus match{
          case Some(_) => "-"
          case None => ""
        }) + digits.reduce(_ + _)
      ))
    }
  }


  def parseFunctionBodyExpr : Parser[Expr] = {
    parseStringLit(Nil)
  }

  def parseFunction : Parser[Function] = {
    (literal("$$") ~> parseStringLit(forbidExtraSymbols = symbolNameForbids) <~
      literal("(")) ~ (((repsep(parseStringLit(forbidExtraSymbols = symbolNameForbids), literal(",")) <~ literal(")")) <~ parseNewLine) ~
      (parseFunctionBodyExpr ~ rep(parseNewLine ~> parseFunctionBodyExpr))) ^^{
      case name ~ (args ~ (first ~ rest)) =>
        val mergedBody = first :: rest
        Function(name.str, args, mergedBody)
    }
  }


  def parseEntryBodyExpr : Parser[Expr] = {
    parseStringLit(Nil)
  }

  def parseEntry : Parser[Entry] = {
    (literal("#") ~> parseStringLit(forbidExtraSymbols = symbolNameForbids) <~ parseNewLine) ~
      (parseEntryBodyExpr ~ rep(parseNewLine ~> parseEntryBodyExpr)) ^^ {
      case name ~ (x ~ xs) =>
        Entry(name.str, x :: xs)
    }
  }


  def parseCheckListBodyExpr : Parser[Expr] = {
    parseFunction | parseEntry | parseStringLit(Nil, allowInterpolators = true)
  }

  def parseCheckList : Parser[CheckList] = {
    (literal("##") ~> parseStringLit(Nil) <~ parseNewLine) ~
      (parseCheckListBodyExpr ~ rep(parseNewLine ~> parseCheckListBodyExpr)) ^^ {
      case name ~ (x ~ xs) =>
        CheckList(name.str, x :: xs)
    }
  }

  def parseFully(str : String) : ParseResult[CheckList] = {
    parseAll(parseCheckList,str)
  }

}
