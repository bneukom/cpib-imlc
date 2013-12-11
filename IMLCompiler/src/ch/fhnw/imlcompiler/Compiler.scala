package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import scala.text._
import Document._
import ch.fhnw.imlcompiler.parsing.IMLParsers
import ch.fhnw.imlcompiler.checking.FlowAnalysis
import ch.fhnw.imlcompiler.transforming.ASTTransformers
import ch.fhnw.codegen.JVMByteCodeGen

// TODO what are default modes?
object Compiler extends IMLParsers with SemanticAnalysis with FlowAnalysis with ASTTransformers with JVMByteCodeGen {

  def main(args: Array[String]) {
    val file = scala.io.Source.fromFile("programs/listcomprehensions2.iml")
    val imlcode = file.mkString
    file.close()

    try {
      // parse
      val program = parse(imlcode)
      println("Parse Successful:")
      println(program.treeString)
      println()

      // context check
      val context = checkSemantics(program);
      println("Semantic Analysis Successful\n")

      // check for possible flow errors (const and initialized)
      ceckFlow(program, context);
      println("Flow Analysis Successful\n")

      // code transformations (for example list expressions)
      val transformed = transform(program, context);
      println("AST Transformed\n")
      println(transformed.treeString)

      // generate appropriate code
      generateCode(transformed);
      println("Byte Code Sucessfully Generated");

    } catch {
      case e: ParseException => { System.err.println(e.getMessage); }
      case e: CompilerException => { System.err.println(e.getMessage); }
    }
  }

}