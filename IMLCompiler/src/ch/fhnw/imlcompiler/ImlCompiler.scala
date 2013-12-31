package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import scala.text._
import Document._
import ch.fhnw.imlcompiler.parsing.IMLParsers
import ch.fhnw.imlcompiler.transforming.ASTTransformers
import ch.fhnw.codegen.JVMByteCodeGen
import java.io.File
import ch.fhnw.imlcompiler.checking.ContextChecker
import ch.fhnw.imlcompiler.checking.CompilerException

// TODO what are default modes?
// TODO implement commandline interface
object ImlCompiler extends IMLParsers with ContextChecker with ASTTransformers with JVMByteCodeGen {

  def main(args: Array[String]) {
    compile("programs/boollisttest.iml", true)
  }

  def compile(fileName: String, debug: Boolean) = {
    val file = scala.io.Source.fromFile(fileName)
    val imlcode = file.mkString
    file.close()

    try {
      // parse
      val program = parse(imlcode)

      if (debug) {
        println("Parse Successful:")
        println(program)
        println()
      }

      // context check
      val symbolTable = contextCheck(program);

      if (debug) {
        println("Context Checking Successful")
        println()
      }
      
      // code transformations (for example list expressions)
      val transformed = transform(program, symbolTable);

      if (debug) {
        println("AST Transformed\n")
        println((transformed))
        println()
      }
      
      // generate jvm byte code
      val path = new File(fileName).getParentFile().getAbsolutePath();
      writeCode(transformed, symbolTable, path);

      if (debug) {
        println("Byte Code Sucessfully Generated");
      }

    } catch {
      case e: ParseException => { System.err.println(e.getMessage); }
      case e: CompilerException => { System.err.println(e.getMessage); }
    }
  }

}