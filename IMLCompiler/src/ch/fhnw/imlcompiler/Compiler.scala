package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST._
import scala.text._
import Document._

// TODO what are default modes?
object Compiler extends IMLParsers with SemanticAnalysis with ASTTransformers {

  def main(args: Array[String]) {
    val file = scala.io.Source.fromFile("programs/listsum.iml")
    val imlcode = file.mkString
    file.close()

    try {
      // parse
      val parseResult = parse(imlcode)
      println("Parse Successful:")
      println(parseResult.treeString)
      println()

      // context check
      val context = check(parseResult);
      
      // TODO flow analysis
      
      // TODO code transformation

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