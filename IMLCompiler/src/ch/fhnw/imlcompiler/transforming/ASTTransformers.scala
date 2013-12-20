package ch.fhnw.imlcompiler.transforming

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST.Expr
import ch.fhnw.imlcompiler.AST._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import ch.fhnw.imlcompiler.ContextChecker
import ch.fhnw.imlcompiler.SymbolTable

// http://en.wikipedia.org/wiki/Set-builder_notation
// TODO either always use List or always ListBuffer...
// TODO type inference of list via return type of list outputfunction
trait ASTTransformers {

  def transform(program: Program, symbolTable: SymbolTable): Program = {

    val transformedDecls = transformDecls(program.cpsDecl)(symbolTable)
    val transformedCmds = transformCommands(program.commands)(symbolTable)

    return Program(program.name, program.params, transformedDecls ++ transformedCmds._1, transformedCmds._2)
  }

  def transformDecls(decls: List[Decl])(implicit symbolTable: SymbolTable): List[Decl] =
    decls.foldRight(List[Decl]())((decl, l) => decl match {
      case s: StoreDecl => s :: l
      case p: ProcDecl => {
        val transformed = transformCommands(p.cmds);
        ProcDecl(p.ident, p.params, p.globImpList, p.decls, transformed._2) :: transformed._1 ::: l
      }
      case f: FunDecl => {
        val transformed = transformCommands(f.cmds);
        FunDecl(f.ident, f.params, f.returns, f.importList, f.cpsDecl, transformed._2) :: transformed._1 ::: l
      }
    })

  def transformCommands(cmds: List[Cmd])(implicit symbolTable: SymbolTable): (List[Decl], List[Cmd]) = {
    val mapped = cmds.map(cmd => {
      cmd match {
        case WhileCmd(e, c) => {
          val transformedExpr = transformExpr(e)
          (transformedExpr._3, WhileCmd(transformedExpr._1, c) :: transformedExpr._2)
        }
        case BecomesCmd(el, er) => {
          val transformedEr = transformExpr(er)
          (transformedEr._3, BecomesCmd(el, transformedEr._1) :: transformedEr._2)
        }
        case IfCmd(e, ifcmd, elsecmd) => {
          val transformedE = transformExpr(e)
          val transformedif = transformCommands(ifcmd)
          val transformedelse = transformCommands(elsecmd)

          (transformedE._3 ++ transformedif._1 ++ transformedelse._1, IfCmd(transformedE._1, transformedif._2, transformedelse._2) :: Nil)
        }
        case OutputCmd(e) => {
          val transformedExpr = transformExpr(e)
          (transformedExpr._3, OutputCmd(transformedExpr._1) :: transformedExpr._2)
        }
      }
    })

    (mapped.map(_._1).flatten, mapped.map(_._2).flatten)
  }

  def transformExpr(expr: Expr)(implicit symbolTable: SymbolTable): (Expr, List[Cmd], List[Decl]) =
    expr match {
      case l: ListExpr => {
        val listExpr = transformListExpr(l);
        return (listExpr._4, listExpr._3, listExpr._1 :: listExpr._2 :: Nil);
      }
      case DyadicExpr(lhs, op, rhs) => {
        val transformedLhs = transformExpr(lhs)
        val transformedRhs = transformExpr(rhs)
        return (DyadicExpr(transformedLhs._1, op, transformedRhs._1), transformedLhs._2 ++ transformedRhs._2, transformedLhs._3 ++ transformedRhs._3)
      }
      case MonadicExpr(rhs, op) => {
        val transformed = transformExpr(rhs);
        return (MonadicExpr(transformed._1, op), transformed._2, transformed._3)
      }
      case l: LiteralExpr => (l, Nil, Nil)
      case s: StoreExpr => (s, Nil, Nil)
      case f: FunCallExpr => {
        val fold = f.e.l.foldRight(List[(Expr, List[Cmd], List[Decl])]())((expr, l) => transformExpr(expr) :: l);
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
  def transformListExpr(lexpr: ListExpr)(implicit symbolTable: SymbolTable): (StoreDecl, StoreDecl, List[Cmd], StoreExpr) = {
    val countIdent = Ident("$" + lexpr.i.value + count)
    val listIdent = Ident("$$l" + count)
    count += 1 // TODO nope

    val counterStoreDecl = StoreDecl(Var, TypedIdent(countIdent, IntType))
    val listStoreDecl = StoreDecl(Var, TypedIdent(listIdent, ListType(IntType)))
    val countInitCmd = BecomesCmd(StoreExpr(countIdent, true), lexpr.to)
    val listInitCmd = BecomesCmd(StoreExpr(listIdent, true), LiteralExpr(ListLiteral(List())))

    val appenderCmd = BecomesCmd(StoreExpr(listIdent, false), DyadicExpr(lexpr.output, ConsOpr, StoreExpr(listIdent, false)))

    val whereCmd = IfCmd(lexpr.predicate, appenderCmd :: Nil, SkipCmd() :: Nil)

    val incrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), PlusOpr, LiteralExpr(IntLiteral(1))))
    val decrementer = BecomesCmd(StoreExpr(countIdent, false), DyadicExpr(StoreExpr(countIdent, false), MinusOpr, LiteralExpr(IntLiteral(1))))

    val lowToHighWhile = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), GE, lexpr.from), whereCmd :: decrementer :: Nil)
    val highToLowWhile = WhileCmd(DyadicExpr(StoreExpr(countIdent, false), LE, lexpr.from), whereCmd :: incrementer :: Nil)

    val topDownSelector = IfCmd(DyadicExpr(lexpr.to, GT, lexpr.from), lowToHighWhile :: Nil, highToLowWhile :: Nil)

    val resultExpr = StoreExpr(listIdent, false)

    return (counterStoreDecl, listStoreDecl, countInitCmd :: listInitCmd :: topDownSelector :: Nil, resultExpr)
  }

  var count = 0;

}