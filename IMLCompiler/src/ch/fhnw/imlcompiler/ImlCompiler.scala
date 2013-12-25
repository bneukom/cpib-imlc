package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import scala.text._
import Document._
import ch.fhnw.imlcompiler.parsing.IMLParsers
import ch.fhnw.imlcompiler.transforming.ASTTransformers
import ch.fhnw.codegen.JVMByteCodeGen

// TODO what are default modes?
// TODO implement commandline interface
object ImlCompiler extends IMLParsers with ContextChecker with ASTTransformers with JVMByteCodeGen {

  def main(args: Array[String]) {

    val file = scala.io.Source.fromFile("programs/aacodegentest.iml")
    val imlcode = file.mkString
    file.close()

    try {
      // parse
      val program = parse(imlcode)
      println("Parse Successful:")
      println(program)
      println()

      // context check
      val symbolTable = contextCheck(program);
      println("Context Checking Successful")
      println()

      // code transformations (for example list expressions)
      val transformed = transform(program, symbolTable);
      println("AST Transformed\n")
      println((transformed))
      println()

      // generate jvm byte code
      writeCode(transformed, symbolTable);
      println("Byte Code Sucessfully Generated");

    } catch {
      case e: ParseException => { System.err.println(e.getMessage); }
      case e: CompilerException => { System.err.println(e.getMessage); }
    }
  }

}