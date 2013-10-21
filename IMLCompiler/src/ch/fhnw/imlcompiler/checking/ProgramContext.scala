package ch.fhnw.imlcompiler.checking

import scala.collection.mutable.MutableList
import scala.collection.mutable.HashMap
import ch.fhnw.imlcompiler.AST.TypedIdent
import ch.fhnw.imlcompiler.AST.MechMode
import ch.fhnw.imlcompiler.AST.ChangeMode
import ch.fhnw.imlcompiler.AST.FlowMode
import ch.fhnw.imlcompiler.AST.Ident
import ch.fhnw.imlcompiler.AST.Decl

object ProgramContext {
  case class Context(globalStoreScope: GlobalStoreScope, localStoreScope: LocalStoreScopes, globalMethodScope: GlobalMethodScope)

  case class Store(typedIdent: TypedIdent, mech: Option[MechMode], change: Option[ChangeMode], flow: Option[FlowMode], var initialzed: Boolean)

  case class GlobalStoreScope(scope: MutableList[Store])
  case class LocalStoreScopes(scope: HashMap[Ident, MutableList[Store]]) // includes global imports and parameters
  case class GlobalMethodScope(decls: HashMap[Ident, Decl])
}
