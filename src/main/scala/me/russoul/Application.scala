package me.russoul

import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util.Scanner

import CheckListParser._
import CheckListAST._
import CheckListInterpreter._


object Application extends App{ //TODO write console application as required


  def run(): Unit ={



    val file = "test2.txt"
    val str = readFile(file).trim //string must not end on new line or any whitespace
    println("=========== FILE ============")
    println(str)
    println("==============================")
    val res = parse(parseFully(parseCheckList), removeComments(str).trim)

    println(res)


    if(res.successful){
      new Interpreter(res.get, System.out, new Scanner(System.in)).interpret() match{
        case Left(err) => println(Console.RED + err + Console.RESET)
        case Right(ok) => println(ok)
      }
    }
  }


  run()



}
