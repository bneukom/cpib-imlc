package ch.fhnw.imlcompiler.transforming

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST.Expr
import ch.fhnw.imlcompiler.AST._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import ch.fhnw.imlcompiler.checking.ContextChecker
import ch.fhnw.imlcompiler.checking.SymbolTable
import ch.fhnw.imlcompiler.checking.Scope
import ch.fhnw.imlcompiler.checking.Store

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
          val transformedCommands = transformCommands(c, scope)

          (transformedExpr._3 ++ transformedCommands._1, transformedExpr._2 :+ WhileCmd(transformedExpr._1, transformedCommands._2))
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
        case InputCmd(e) => {
          val transformedExpr = transformExpr(e, scope)
          (transformedExpr._3, transformedExpr._2 :+ InputCmd(transformedExpr._1))
        }
        case c: CallCmd => {
          val fold = c.e.l.foldRight(List[(Expr, List[Cmd], List[Decl])]())((expr, l) => transformExpr(expr, scope) :: l);
          (fold.map(_._3).flatten, fold.map(_._2).flatten :+ CallCmd(c.i, TupleExpr(fold.map(_._1))))
        }
        case s: SkipCmd => (Nil, Nil)
      }
    })

    (mapped.map(_._1).flatten, mapped.map(_._2).flatten)
  }

  def transformExpr(expr: Expr, scope: Scope)(implicit symbolTable: SymbolTable): (Expr, List[Cmd], List[Decl]) =
    expr match {
      case l: ListComprehension => {
        val listExpr = transformListExpr(l, scope);
        return (listExpr._3, listExpr._2, listExpr._1);
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

  // TODO nested comprehensions do not work right
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
  def transformListExpr(lexpr: ListComprehension, scope: Scope)(implicit symbolTable: SymbolTable): (List[Decl], List[Cmd], StoreExpr) = {
    val countIdent = Ident("$" + lexpr.i.value)
    val listIdent = Ident("$$l")

    val countTypedIdent = TypedIdent(countIdent, IntType);
    val listIdentTypedIdent = TypedIdent(listIdent, ListType(IntType)) // TODO maybe infer type?

    scope += Store(countTypedIdent, None, Some(Var), None, synthetic = true);
    scope += Store(listIdentTypedIdent, None, Some(Var), None, synthetic = true);

    val counterStoreDecl = StoreDecl(Var, countTypedIdent)
    val listStoreDecl = StoreDecl(Var, listIdentTypedIdent)
    val countInitCmd = BecomesCmd(StoreExpr(countIdent, true), lexpr.to)
    val listInitCmd = BecomesCmd(StoreExpr(listIdent, true), LiteralExpr(ListLiteral(List())))

    val appenderCmd = BecomesCmd(StoreExpr(listIdent, false), DyadicExpr(replace(transformExpr(lexpr.output, scope)._1, lexpr.i, countIdent), ConsOpr, StoreExpr(listIdent, false)))

    val transformedPredicate = transformExpr(lexpr.predicate, scope);
    val whereCmd = IfCmd(replace(transformedPredicate._1, lexpr.i, countIdent), appenderCmd :: Nil, SkipCmd() :: Nil)

    val incrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), PlusOpr, LiteralExpr(IntLiteral(1))))
    val decrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), MinusOpr, LiteralExpr(IntLiteral(1))))

    val transformedFrom = transformExpr(lexpr.from, scope);
    val transformedTo = transformExpr(lexpr.to, scope);

    val lowToHighWhile = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), GE, transformedFrom._1), whereCmd :: decrementer :: Nil)
    val highToLowWhile = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), LT, transformedTo._1), whereCmd :: incrementer :: Nil)

    val topDownSelector = IfCmd(DyadicExpr(transformedTo._1, GT, transformedFrom._1), lowToHighWhile :: Nil, highToLowWhile :: Nil)

    val resultExpr = StoreExpr(listIdent, false)

    return (transformedPredicate._3 ++ (counterStoreDecl :: listStoreDecl :: Nil), transformedPredicate._2 ++ (countInitCmd :: listInitCmd :: topDownSelector :: Nil), resultExpr)
  }

  def replace(e: Expr, from: Ident, to: Ident): Expr = {
    e match {
      case d: DyadicExpr => DyadicExpr(replace(d.l, from, to), d.op, replace(d.r, from, to))
      case s: StoreExpr => StoreExpr(if (s.i == from) to else s.i, s.isInitialization);
      case l: LiteralExpr => l;
      case f: FunCallExpr => FunCallExpr(f.i, TupleExpr(f.e.l.map(replace(_, from, to))))
      case m: MonadicExpr => MonadicExpr(replace(m.l, from, to), m.op)
      case l: ListComprehension => ListComprehension(replace(l.output, from, to), l.i, replace(l.from, from, to), replace(l.to, from, to), replace(l.predicate, from, to))
    }
  }

}