package ch.fhnw.imlcompiler.checking

import scala.collection.mutable.MutableList
import scala.collection.mutable.HashMap
import ch.fhnw.imlcompiler.AST.TypedIdent
import ch.fhnw.imlcompiler.AST.MechMode
import ch.fhnw.imlcompiler.AST.ChangeMode
import ch.fhnw.imlcompiler.AST.FlowMode
import ch.fhnw.imlcompiler.AST.Ident
import ch.fhnw.imlcompiler.AST.Decl
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer

// TODO into SemanticAnalysis.scala?
// TODO really object??!!
object ProgramContext {
  case class Context(globalStoreScope: GlobalStoreScope, localStoreScope: LocalStoreScopes, globalMethodScope: GlobalMethodScope)
  
  case class Store(typedIdent: TypedIdent, mech: Option[MechMode], change: Option[ChangeMode], flow: Option[FlowMode])
  case class GlobalStoreScope(scope: ListBuffer[Store])
  case class LocalStoreScopes(scope: HashMap[Ident, ListBuffer[Store]]) // includes global imports and parameters
  case class GlobalMethodScope(decls: HashMap[Ident, Decl])
}
