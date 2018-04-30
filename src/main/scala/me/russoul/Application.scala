package me.russoul

import java.io.File
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util.Scanner

import CheckListParser._
import CheckListAST._
import CheckListInterpreter._


object Application{


  val LOG : Boolean = true


  def main(args : Array[String]): Unit ={

    var srcFilename : Option[String] = None
    var inputFilename : Option[String] = None

    var in = new Scanner(System.in)


    def handle(): Unit ={
      val file = new File(srcFilename.get)

      inputFilename match{
        case None => ()
        case Some(filename) =>
          val file = new File(filename)
          if(!file.exists()){
            println(Console.RED + "input file not found" + Console.RESET)
            return
          }
          in = new Scanner(file)
      }

      if(!file.exists()){
        println(Console.RED + "script file not found" + Console.RESET)
        return
      }

      val contents = removeComments(readFile(file.getAbsolutePath, Charset.defaultCharset())).trim
      val parsed = parse(parseFully(parseCheckList), contents)
      parsed match{
        case Error(_, _) =>
          println(Console.RED + "could not parse the script" + Console.RESET)
          println(parsed)
        case Success(res, _) =>
          println(Console.GREEN + "parsed the script successfully" + Console.RESET)
          new Interpreter(res, System.out, in).interpret() match{
            case Left(err) =>
              println(Console.RED + "could not interpret the script" + Console.RESET)
              println(Console.RED + err + Console.RESET)
            case Right(ok) =>
              println(Console.GREEN + "ALL DONE" + Console.RESET)
              println(ok)
          }

      }
    }

    if(args.isEmpty){
      println(Console.YELLOW+"Checklist v1.0"+Console.RESET)
      println("Use `--help` for details")
      return
    }

    if(args.length == 1){
      if(args(0) == "--help"){
        println("Usage:")
        println("[--file=<filename>] <src>")
        println("Examples:")
        println("my_script.txt")
        println("^^^ will be interpreted in interactive mode")
        println("--file=my_input.txt my_script.txt")
        println("^^^ all input will be read from the specified file")
        return
      }else{
        srcFilename = Some(args(0))
      }
    }else if (args.length == 2){
      for(arg <- args){
        if(arg.startsWith("--file=")){
          if(inputFilename.isDefined){
            println("incorrect arguments")
            println("Use `--help` for details")
            return
          }
          val file = arg.replaceAll("\\-\\-file\\=", "")
          inputFilename = Some(file)
        }else{
          if(srcFilename.isDefined){
            println("incorrect arguments")
            println("Use `--help` for details")
            return
          }
          srcFilename = Some(arg)
        }
      }
    }else{
      println("incorrect arguments")
      println("Use `--help` for details")
      return
    }


    handle()
  }




}
