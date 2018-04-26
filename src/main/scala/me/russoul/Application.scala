package me.russoul

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util.Scanner

import CheckListParser._
import CheckListAST._
import CheckListInterpreter._

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.OffsetPosition

object Application extends App{




  def readFile(path: String, encoding: Charset = Charset.defaultCharset()) = {
    val encoded = Files.readAllBytes(Paths.get(path))
    new String(encoded, encoding)
  }




  case class TestCase[T](parser : Parser[T], str : String, suc : (ParseResult[T] => Boolean)){
    val res = parseAll(parser,str)
    println("-----------------------")
    println("Input:\n" + str)
    println("Parse result: ")
    println(res)
    if(suc(res)){
      println(s"Valid: ${Console.GREEN}YES${Console.RESET}")
    }else{
      println(s"Valid: ${Console.RED}NO${Console.RESET}")
      System.exit(0)
    }
    println("-----------------------")
  }

  def test(): Unit ={

    println("=========== TESTS ============")
    TestCase[StringLit](parseStringLit(Nil), "-125", x => x.successful && x.get.str.toInt == -125) //TODO remove Int and Bool literals ?
    TestCase[StringLit](parseStringLit(Nil), "99", x => x.successful && x.get.str.toInt == 99)
    TestCase[StringLit](parseStringLit(Nil), "t", x => x.successful)
    TestCase[StringLit](parseStringLit(Nil), "молоко 15 штук", x => x.successful)
    TestCase[StringLit](parseStringLit(Nil, allowInterpolators = false), "молоко ${} штук", x => !x.successful)
    TestCase[StringLit](parseStringLit(Nil, allowInterpolators = true), "молоко ${15} штук", x => x.successful)
    TestCase[StringLit](parseStringLit(Nil, allowInterpolators = true), "молоко ${${1}} штук", x => x.successful)
    TestCase[StringLit](parseStringLit(Nil, allowInterpolators = true), "молоко ${15} штук + еще ${2} сверху", x => x.successful)
    TestCase[Expr](parseCheckList, "##test checklist\nhey !\nthis is checklist ()", x => x.successful)
    TestCase[Expr](parseFunctionBodyExpr, "##test checklist\nhey !\nthis is checklist ()", x => !x.successful)
    TestCase[Expr](parseFunctionBodyExpr, "this is checklist () ${1}", x => x.successful)
    TestCase[CheckList](parseCheckList, "##test checklist\nhey !\n$$fun1(name)\n    ${hey !}\n    another hey !\nhey2 !\n#clothers\n    cloth1\n    cloth2\nstuff\nstuff\nstuff\n$fun1(${1})", x => x.successful)
    TestCase[Conditional](parseConditional, "$if{true}\n   ok!", x => x.successful)
    TestCase[CheckList](parseCheckList, "##test\n$if{$tr}\n   ok!", x => x.successful)
    TestCase[Expr](parseStringLit(Nil, allowInterpolators = true), "$var is true !", x => x.successful)
    TestCase[Expr](parseStringLit(Nil), "boom -> het", x => !x.successful)
    TestCase[CheckList](parseCheckList, "##checklist\n<- hey!", x => x.successful)
    TestCase[List[Expr]](parseElseBranch(0), "$else\n    else branch", x => x.successful)
    println("==============================")

    val file = "test1.txt"
    val str = readFile(file).trim //string must not end on new line or any whitespace
    println("=========== FILE ============")
    println(str)
    println("==============================")
    val res = parseAll(parseCheckList, str)

    println(res)


    if(res.successful){
      interpret(res.get, System.out, new Scanner(System.in)) match{
        case Left(err) => println(Console.RED + err + Console.RESET)
        case Right(ok) => println(ok)
      }
    }



  }

  test()



}
