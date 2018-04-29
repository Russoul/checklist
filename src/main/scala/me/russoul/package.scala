package me


import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util.Scanner

import me.russoul.CheckListParser._
import me.russoul.CheckListAST._
import me.russoul.CheckListInterpreter._

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.OffsetPosition

package object russoul {

  def readFile(path: String, encoding: Charset = Charset.defaultCharset()): String = {
    val encoded = Files.readAllBytes(Paths.get(path))
    new String(encoded, encoding)
  }

  case class TestCaseParser[T](parser : Parser[T], str : String, suc : (ParseResult[T] => Boolean)){
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


  case class TestCaseInterpreter(parser : Parser[CheckList], toParse : String, in : Scanner, correctResult : String){
    val res = parse(parseFully(parser),removeComments(toParse).trim)
    println("-----------------------")
    println("Input:\n" + toParse)
    println("Parse result: ")
    println(res)
    if(res.successful){
      println(s"PARSED: ${Console.GREEN}YES${Console.RESET}")
      val ast = res.get
      new Interpreter(ast, System.out, in).interpret() match{
        case Right(ok) =>
          println(s"INTERPRETED: ${Console.GREEN}YES${Console.RESET}")
          if(ok.trim == correctResult.trim){
            println(s"VALID: ${Console.GREEN}YES${Console.RESET}")
            in.close()
          }else{
            println(s"VALID: ${Console.RED}NO${Console.RESET}")
            println("got result: ")
            println(ok)
            System.exit(0)
          }
        case Left(err) =>
          println(s"INTERPRETED: ${Console.RED}NO${Console.RESET}")
          println("error:")
          System.out.println(err)
          System.exit(0)
      }
    }else{
      println(s"PARSED: ${Console.RED}NO${Console.RESET}")
      System.exit(0)
    }
    println("-----------------------")
  }

  val successOnParseSuccess = (res : ParseResult[Object]) => res.successful
  val successOnParseFail = (res : ParseResult[Object]) => !res.successful

}
