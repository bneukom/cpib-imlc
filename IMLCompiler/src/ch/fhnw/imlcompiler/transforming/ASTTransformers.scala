package ch.fhnw.imlcompiler.transforming

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST.Expr
import ch.fhnw.imlcompiler.AST._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import ch.fhnw.imlcompiler.ContextChecker
import ch.fhnw.imlcompiler.SymbolTable
import ch.fhnw.imlcompiler.Scope
import ch.fhnw.imlcompiler.Store

// http://en.wikipedia.org/wiki/Set-builder_notation
// TODO either always use List or always ListBuffer...
// TODO type inference of list via return type of list outputfunction
trait ASTTransformers {

  def transform(program: Program, symbolTable: SymbolTable): Program = {

    val transformedDecls = transformDecls(program.cpsDecl)(symbolTable)
    val transformedCmds = transformCommands(program.commands, symbolTable.globalScope)(symbolTable)

    return Program(program.name, program.params, transformedDecls ++ transformedCmds._1, transformedCmds._2)
  }

  def transformDecls(decls: List[Decl])(implicit symbolTable: SymbolTable): List[Decl] =
    decls.foldRight(List[Decl]())((decl, l) => decl match {
      case s: StoreDecl => s :: l
      case p: ProcDecl => {
        val transformed = transformCommands(p.cmds, symbolTable.getLocalStoreScope(p.ident));
        ProcDecl(p.ident, p.params, p.globImpList, p.decls, transformed._2) :: transformed._1 ::: l
      }
      case f: FunDecl => {
        val transformed = transformCommands(f.cmds, symbolTable.getLocalStoreScope(f.ident));
        FunDecl(f.ident, f.params, f.returns, f.importList, f.cpsDecl, transformed._2) :: transformed._1 ::: l
      }
    })

  def transformCommands(cmds: List[Cmd], scope: Scope)(implicit symbolTable: SymbolTable): (List[Decl], List[Cmd]) = {
    val mapped = cmds.map(cmd => {
      cmd match {
        case WhileCmd(e, c) => {
          val transformedExpr = transformExpr(e, scope)
          (transformedExpr._3, transformedExpr._2 :+ WhileCmd(transformedExpr._1, c))
        }
        case BecomesCmd(el, er) => {
          val transformedEr = transformExpr(er, scope)
          (transformedEr._3, transformedEr._2 :+ BecomesCmd(el, transformedEr._1))
        }
        case IfCmd(e, ifcmd, elsecmd) => {
          val transformedE = transformExpr(e, scope)
          val transformedif = transformCommands(ifcmd, scope)
          val transformedelse = transformCommands(elsecmd, scope)

          (transformedE._3 ++ transformedif._1 ++ transformedelse._1, IfCmd(transformedE._1, transformedif._2, transformedelse._2) :: Nil)
        }
        case OutputCmd(e) => {
          val transformedExpr = transformExpr(e, scope)
          (transformedExpr._3, transformedExpr._2 :+ OutputCmd(transformedExpr._1))
        }
      }
    })

    (mapped.map(_._1).flatten, mapped.map(_._2).flatten)
  }

  def transformExpr(expr: Expr, scope: Scope)(implicit symbolTable: SymbolTable): (Expr, List[Cmd], List[Decl]) =
    expr match {
      case l: ListExpr => {
        val listExpr = transformListExpr(l, scope);
        return (listExpr._4, listExpr._3, listExpr._1 :: listExpr._2 :: Nil);
      }
      case DyadicExpr(lhs, op, rhs) => {
        val transformedLhs = transformExpr(lhs, scope)
        val transformedRhs = transformExpr(rhs, scope)
        return (DyadicExpr(transformedLhs._1, op, transformedRhs._1), transformedLhs._2 ++ transformedRhs._2, transformedLhs._3 ++ transformedRhs._3)
      }
      case MonadicExpr(rhs, op) => {
        val transformed = transformExpr(rhs, scope);
        return (MonadicExpr(transformed._1, op), transformed._2, transformed._3)
      }
      case l: LiteralExpr => (l, Nil, Nil)
      case s: StoreExpr => (s, Nil, Nil)
      case f: FunCallExpr => {
        val fold = f.e.l.foldRight(List[(Expr, List[Cmd], List[Decl])]())((expr, l) => transformExpr(expr, scope) :: l);
        return (FunCallExpr(f.i, TupleExpr(fold.map(_._1))), fold.map(_._2).flatten, fold.map(_._3).flatten)
      }
    }

  // TODO does not work quite right probably if with two while loops (one for > and one for <) needed
  // TODO update scope ?
  // TODO what about nested list comprehensions?
  // TODO from 2 to 100 we need to start with 100 and go back for correct list order [2,3,4,5,...100]
  /**
   * Roughly translates a list expr into the following code (AST).
   *
   * var $x1:int;
   * var $$l1:[int];
   * $x1 init := toExpr;
   * $$l1 init := []
   *
   * primes init := $$l1;
   */
  def transformListExpr(lexpr: ListExpr, scope: Scope)(implicit symbolTable: SymbolTable): (StoreDecl, StoreDecl, List[Cmd], StoreExpr) = {
    val countIdent = Ident("$" + lexpr.i.value + count)
    val listIdent = Ident("$$l" + count)
    count += 1 // TODO nope

    val countTypedIdent = TypedIdent(countIdent, IntType);
    val listIdentTypedIdent = TypedIdent(listIdent, ListType(IntType)) // TODO maybe infer type?

    scope += Store(countTypedIdent, None, Some(Var), None);
    scope += Store(listIdentTypedIdent, None, Some(Var), None);

    val counterStoreDecl = StoreDecl(Var, countTypedIdent)
    val listStoreDecl = StoreDecl(Var, listIdentTypedIdent)
    val countInitCmd = BecomesCmd(StoreExpr(countIdent, true), lexpr.to)
    val listInitCmd = BecomesCmd(StoreExpr(listIdent, true), LiteralExpr(ListLiteral(List())))

    val appenderCmd = BecomesCmd(StoreExpr(listIdent, false), DyadicExpr(replace(lexpr.output, lexpr.i, countIdent), ConsOpr, StoreExpr(listIdent, false)))

    val whereCmd = IfCmd(replace(lexpr.predicate, lexpr.i, countIdent), appenderCmd :: Nil, SkipCmd() :: Nil)

    val incrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), PlusOpr, LiteralExpr(IntLiteral(1))))
    val decrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), MinusOpr, LiteralExpr(IntLiteral(1))))

    val lowToHighWhile = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), GE, lexpr.from), whereCmd :: decrementer :: Nil)
    val highToLowWhile = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), LE, lexpr.from), whereCmd :: incrementer :: Nil)

    val topDownSelector = IfCmd(DyadicExpr(lexpr.to, GT, lexpr.from), lowToHighWhile :: Nil, highToLowWhile :: Nil)

    val resultExpr = StoreExpr(listIdent, false)

    return (counterStoreDecl, listStoreDecl, countInitCmd :: listInitCmd :: topDownSelector :: Nil, resultExpr)
  }

  def replace(e: Expr, from: Ident, to: Ident): Expr = {
    e match {
      case d: DyadicExpr => DyadicExpr(replace(d.l, from, to), d.op, replace(d.l, from, to))
      case s: StoreExpr => StoreExpr(if (s.i == from) to else s.i, s.isInitialization);
      case l: LiteralExpr => l;
      case f: FunCallExpr => FunCallExpr(f.i, TupleExpr(f.e.l.map(replace(_, from, to))))
    }
  }

  var count = 0;

}