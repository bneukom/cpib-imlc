package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST.ASTNode
import ch.fhnw.imlcompiler.AST.Expr

class CompilerException(v:String) extends RuntimeException(v) {
  def this(n:ASTNode) = this(n.pos.longString)
}