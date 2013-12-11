package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import scala.text._
import Document._
import ch.fhnw.imlcompiler.parsing.IMLParsers
import ch.fhnw.imlcompiler.checking.FlowAnalysis
import ch.fhnw.imlcompiler.transforming.ASTTransformers

// TODO what are default modes?
object Compiler extends IMLParsers with SemanticAnalysis with FlowAnalysis with ASTTransformers {

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

      // TODO interpret
    } catch {
      //      case e: ParseException => { e.printStackTrace() }
      //      case e: CompilerException => { e.printStackTrace() }
      case e: ParseException => { System.err.println(e.getMessage); }
      case e: CompilerException => { System.err.println(e.getMessage); }
    }

    //    val lit = parser("[[[], [[3]]]]", listLiteral)
    //    println(lit + ": " + listType(lit, 0))
  }

}