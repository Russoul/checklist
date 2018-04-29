package me.russoul

import java.io.File
import java.util.Scanner

import me.russoul.CheckListAST.{CheckList, Conditional, Expr, StringExpr}
import me.russoul.CheckListParser.{parseCheckList, parseConditional, parseElseBranch, parseFunction, parseFunctionBodyExpr, parseStringExpr, parseStringInterpolator, parseUnOperatorInsideInterpolator}

object Test extends App{

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
        TestCaseParser[Object](parseCheckList, content, successOnParseSuccess)
      }


      )
    }

    def testFail(dir : String): Unit ={
      foreachFileInDir(dir,  (path,content) => {
        println("file: " + path)
        TestCaseParser[Object](parseCheckList, content, successOnParseFail)
      }


      )
    }

    testSuccess(success)
    testFail(fail)
  }

  def testInterpreter(): Unit ={ //TODO move to sbt test path
    val dir = s"tests${File.separator}interpreter${File.separator}"

    type FileName = String
    def foreachFileInDir(dir : String, f : (FileName, String, Scanner, String) => Unit): Unit ={
      val folder = new File(dir)
      val files = folder.listFiles().filter(_.isFile).map(_.getName.replaceFirst("[.][^.]+$", "").replaceAll("_.+", ""))
      for(file <- files){
        val content = readFile(dir + file + ".txt")
        val input = new Scanner(readFile(dir + file + "_input.txt"))
        val mustbe = readFile(dir + file + "_result.txt")
        f(dir + file + ".txt", content, input, mustbe)
      }
    }

    def testSuccess(dir : String): Unit ={
      foreachFileInDir(dir,  (path, content, in, mustbe) => {
        println("file: " + path)
        TestCaseInterpreter(parseCheckList, content, in, mustbe)
      }


      )
    }

    testSuccess(dir)
  }

  def test(): Unit ={

    println("=========== EXTRA PARSER TESTS ============")
    TestCaseParser[StringExpr](parseStringExpr(Nil), "-125", x => x.successful && x.get.str.toInt == -125)
    TestCaseParser[StringExpr](parseStringExpr(Nil), "99", x => x.successful && x.get.str.toInt == 99)
    TestCaseParser[StringExpr](parseStringExpr(Nil), "t", x => x.successful)
    TestCaseParser[StringExpr](parseStringExpr(Nil), "молоко 15 штук", x => x.successful)
    TestCaseParser[StringExpr](parseStringExpr(Nil, allowInterpolators = false), "молоко ${} штук", x => !x.successful)
    TestCaseParser[StringExpr](parseStringExpr(Nil, allowInterpolators = true), "молоко ${\"15\"} штук", x => x.successful)
    TestCaseParser[StringExpr](parseStringExpr(Nil, allowInterpolators = true), "молоко ${\"15\"} штук + еще ${\"2\"} сверху", x => x.successful)
    TestCaseParser[Expr](parseCheckList, "##test checklist\nhey !\nthis is checklist ()", x => x.successful)
    TestCaseParser[Expr](parseFunctionBodyExpr, "##test checklist\nhey !\nthis is checklist ()", x => !x.successful)
    TestCaseParser[Expr](parseFunctionBodyExpr, "this is checklist () ${\"1\"}", x => x.successful)
    TestCaseParser[CheckList](parseCheckList, "##test checklist\nhey !\n$$fun1(name)\n    ${\"hey !\"}\n    another hey !\nhey2 !\n#clothers\n    cloth1\n    cloth2\nstuff\nstuff\nstuff\n$fun1(${\"1\"})", x => x.successful)
    TestCaseParser[Conditional](parseConditional, "$if{smth}\n   ok!", x => x.successful)
    TestCaseParser[CheckList](parseCheckList, "##test\n$if{tr}\n   ok!", x => x.successful)
    TestCaseParser[Expr](parseStringExpr(Nil, allowInterpolators = true), "$var is true !", x => x.successful)
    TestCaseParser[Expr](parseStringExpr(Nil), "boom -> het", x => !x.successful)
    TestCaseParser[CheckList](parseCheckList, "##checklist\n<- hey!", x => x.successful)
    TestCaseParser[List[Expr]](parseElseBranch(0), "$else\n    else branch", x => x.successful)
    TestCaseParser[Object](parseFunction, "$$factorial(n)\n    $if{||(==(n, 1), ==(n,0))}\n            1\n    $else\n            $factorial(*(n, -(n,1)))", x => x.successful)
    TestCaseParser[Object](parseStringInterpolator, "${hey}", _.successful)
    TestCaseParser[Object](parseStringInterpolator, "${hey()}", _.successful)
    TestCaseParser[Object](parseStringInterpolator, """${"1" + "2"}""", _.successful)
    TestCaseParser[Object](parseStringInterpolator, """${("1" + "2")}""", _.successful)
    TestCaseParser[Object](parseUnOperatorInsideInterpolator, """-(-(-1) * 5)""", _.successful)
    println("==============================")

    testParser()
    testInterpreter()


  }

  test()

}
