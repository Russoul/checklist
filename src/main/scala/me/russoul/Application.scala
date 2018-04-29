package me.russoul

import java.io.File
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
    val res = parse(parseFully(parser),removeComments(str).trim)
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

  val successOnParseSuccess = (res : ParseResult[Object]) => res.successful
  val successOnParseFail = (res : ParseResult[Object]) => !res.successful

  def testParser(): Unit ={ //TODO move to sbt test path
    val dir = s"tests${File.separator}parser${File.separator}"
    val fail = dir + s"fail${File.separator}"
    val success = dir + s"success${File.separator}"

    type FileName = String
    def foreachFileInDir(dir : String, f : (FileName, String) => Unit): Unit ={
      val folder = new File(dir)
      val files = folder.listFiles().filter(_.isFile)
      for(file <- files){
        val content = readFile(file.getAbsolutePath)
        f(file.getAbsolutePath, content)
      }
    }

    def testSuccess(dir : String): Unit ={
      foreachFileInDir(dir,  (path, content) => {
        println("file: " + path)
        TestCase[Object](parseCheckList, content, successOnParseSuccess)
      }


      )
    }

    def testFail(dir : String): Unit ={
      foreachFileInDir(dir,  (path,content) => {
        println("file: " + path)
        TestCase[Object](parseCheckList, content, successOnParseFail)
      }


      )
    }

    testSuccess(success)
    testFail(fail)
  }

  def test(): Unit ={

    println("=========== TESTS ============")
    TestCase[StringExpr](parseStringExpr(Nil), "-125", x => x.successful && x.get.str.toInt == -125)
    TestCase[StringExpr](parseStringExpr(Nil), "99", x => x.successful && x.get.str.toInt == 99)
    TestCase[StringExpr](parseStringExpr(Nil), "t", x => x.successful)
    TestCase[StringExpr](parseStringExpr(Nil), "молоко 15 штук", x => x.successful)
    TestCase[StringExpr](parseStringExpr(Nil, allowInterpolators = false), "молоко ${} штук", x => !x.successful)
    TestCase[StringExpr](parseStringExpr(Nil, allowInterpolators = true), "молоко ${\"15\"} штук", x => x.successful)
    TestCase[StringExpr](parseStringExpr(Nil, allowInterpolators = true), "молоко ${\"15\"} штук + еще ${\"2\"} сверху", x => x.successful)
    TestCase[Expr](parseCheckList, "##test checklist\nhey !\nthis is checklist ()", x => x.successful)
    TestCase[Expr](parseFunctionBodyExpr, "##test checklist\nhey !\nthis is checklist ()", x => !x.successful)
    TestCase[Expr](parseFunctionBodyExpr, "this is checklist () ${\"1\"}", x => x.successful)
    TestCase[CheckList](parseCheckList, "##test checklist\nhey !\n$$fun1(name)\n    ${\"hey !\"}\n    another hey !\nhey2 !\n#clothers\n    cloth1\n    cloth2\nstuff\nstuff\nstuff\n$fun1(${\"1\"})", x => x.successful)
    TestCase[Conditional](parseConditional, "$if{smth}\n   ok!", x => x.successful)
    TestCase[CheckList](parseCheckList, "##test\n$if{tr}\n   ok!", x => x.successful)
    TestCase[Expr](parseStringExpr(Nil, allowInterpolators = true), "$var is true !", x => x.successful)
    TestCase[Expr](parseStringExpr(Nil), "boom -> het", x => !x.successful)
    TestCase[CheckList](parseCheckList, "##checklist\n<- hey!", x => x.successful)
    TestCase[List[Expr]](parseElseBranch(0), "$else\n    else branch", x => x.successful)
    TestCase[Object](parseFunction, "$$factorial(n)\n    $if{||(==(n, 1), ==(n,0))}\n            1\n    $else\n            $factorial(*(n, -(n,1)))", x => x.successful)
    TestCase[Object](parseStringInterpolator, "${hey}", _.successful)
    TestCase[Object](parseStringInterpolator, "${hey()}", _.successful)
    TestCase[Object](parseStringInterpolator, """${"1" + "2"}""", _.successful)
    TestCase[Object](parseStringInterpolator, """${("1" + "2")}""", _.successful)
    TestCase[Object](parseUnOperatorInsideInterpolator, """-(-(-1) * 5)""", _.successful)
    println("==============================")

    testParser()


  }

  def run(): Unit ={



    val file = "test2.txt"
    val str = readFile(file).trim //string must not end on new line or any whitespace
    println("=========== FILE ============")
    println(str)
    println("==============================")
    val res = parse(parseFully(parseCheckList), removeComments(str).trim)

    println(res)


    if(res.successful){
      interpret(res.get, System.out, new Scanner(System.in)) match{
        case Left(err) => println(Console.RED + err + Console.RESET)
        case Right(ok) => println(ok)
      }
    }
  }

  test()

  run()



}
