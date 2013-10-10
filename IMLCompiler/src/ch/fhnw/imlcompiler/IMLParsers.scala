package ch.fhnw.imlcompiler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import ch.fhnw.imlcompiler.AST._

trait IMLParsers extends RegexParsers {
  def program: Parser[Program] = "program" ~ ident ~ opt("global" ~> cpsDecl) ~ "do" ~ cpsCmd ~ "endprogram" ^^ { case "program" ~ id ~ cpsdecl ~ "do" ~ cmd ~ "endprogram" => Program(cpsdecl, cmd) }

  def atomtype: Parser[Type] = "int" ^^^ { IntType } | "bool" ^^^ { BoolType };

  // commands
  def cmd: Parser[Cmd] = skipCmd | becomesCmd | ifCmd | whileCmd | callCmd | inputCmd | outputCmd
  def skipCmd: Parser[SkipCmd] = "skip" ^^^ { SkipCmd() }
  def becomesCmd: Parser[BecomesCmd] = expr ~ ":=" ~ expr ^^ { case lhs ~ ":=" ~ rhs => BecomesCmd(lhs, rhs) }
  def ifCmd: Parser[IfCmd] = "if" ~ expr ~ "do" ~ cpsCmd ~ "else" ~ cpsCmd ~ "endif" ^^ { case "if" ~ expr ~ "do" ~ c1 ~ "else" ~ c2 ~ "endif" => IfCmd(expr, c1, c2) }
  def whileCmd: Parser[WhileCmd] = "while" ~ expr ~ "do" ~ cpsCmd ~ "endwhile" ^^ { case "while" ~ t1 ~ "do" ~ c ~ "endwhile" => WhileCmd(t1, c) }
  def callCmd: Parser[CallCmd] = "call" ~ ident ~ tupleExpr ^^ { case "call" ~ i ~ t => CallCmd(i, t) }
  def inputCmd: Parser[InputCmd] = "input" ~> expr ^^ { case e => InputCmd(e) }
  def outputCmd: Parser[OutputCmd] = "output" ~> expr ^^ { case e => OutputCmd(e) }

  def cpsCmd: Parser[CpsCmd] = repsep(cmd, ";") ^^ { case cl => CpsCmd(cl) }

  // operators
  def multOpr: Parser[MultOpr] = "*" ^^^ { TimesOpr } | "div" ^^^ { DivOpr } | "mod" ^^^ { ModOpr }
  def boolOpr: Parser[BoolOpr] = "cand" ^^^ { Cand } | "cor" ^^^ { Cor }
  def relOpr: Parser[RelOpr] = "==" ^^^ { EQ } | "!=" ^^^ { NE } | "<=" ^^^ { LE } | ">=" ^^^ { GE } | ">" ^^^ { GT } | "<" ^^^ { LT }
  def addOpr: Parser[AddOpr] = "-" ^^^ { MinusOpr } | "+" ^^^ { PlusOpr }

  // TODO exclude keywords!!
  // TODO change regex for identifier
  def ident: Parser[Ident] = raw"[A-Za-z0-9]+".r.withFailureMessage("identifier expected") ^^ { x => Ident(x.toString()) }

  // literals
  def literal: Parser[Literal] = intLiteral | boolLiteral
  def intLiteral: Parser[IntLiteral] = "[0-9]+".r ^^ { x => { IntLiteral(x.toInt) } }
  def boolLiteral: Parser[BoolLiteral] = ("true" | "false") ^^ { x => BoolLiteral(x.toBoolean) }

  // expressions
  def monadicExpr: Parser[MonadicExpr] = monadicAddExpr | monadicNotExpr;
  def monadicAddExpr: Parser[MonadicExpr] = addOpr ~ factor ^^ { case op ~ exp => MonadicExpr(exp, op) }
  def monadicNotExpr: Parser[MonadicExpr] = "not" ~> factor ^^ { case exp => MonadicExpr(exp, Not) }

  // TODO Store Expr probably wrong?
  def factor: Parser[Expr] = positioned(literal ^^ { LiteralExpr(_) } | ident ~ tupleExpr ^^ { case i ~ r => FunCallExpr(i, r) } | ident ~ "init" ^^ { case i ~ "init" => StoreExpr(i, true) } | ident ^^ { case i => StoreExpr(i, false) } | monadicExpr | "(" ~> expr <~ ")")

  def expr: Parser[Expr] = positioned(term1 * (boolOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) }))
  def term1: Parser[Expr] = positioned(term2 ~ relOpr ~ term2 ^^ { case x1 ~ o ~ x2 => DyadicExpr(x1, o, x2) } | term2 ^^ { case term2 => term2 })
  def term2: Parser[Expr] = positioned(term3 * (addOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) }))
  def term3: Parser[Expr] = positioned(factor * (multOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) }))
  def tupleExpr: Parser[TupleExpr] = positioned("(" ~ repsep(expr, ",") ~ ")" ^^ { case "(" ~ e ~ ")" => TupleExpr(e) })

  def flowMode: Parser[FlowMode] = "in" ^^^ { In } | "out" ^^^ { Out } | "inout" ^^^ { InOut }
  def changeMode: Parser[ChangeMode] = "var" ^^^ { Var } | "const" ^^^ { Const }
  def mechMode: Parser[MechMode] = "ref" ^^^ { Ref } | "copy" ^^^ { Copy }

  // decls
  def globImpList: Parser[GlobImpList] = repsep(globImport, ",") ^^ { case g => GlobImpList(g) }
  def globImport: Parser[GlobImport] = flowMode ~ changeMode ~ ident ^^ { case f ~ c ~ i => GlobImport(f, c, i) }
  def cpsDecl: Parser[CpsDecl] = repsep(decl, ";") ^^ { case dl => CpsDecl(dl) }
  def decl: Parser[Decl] = varDecl | funDecl | procDecl;
  def varDecl: Parser[VarDecl] = changeMode ~ typedIdent ^^ { case c ~ i => VarDecl(c, i) }
  def funDecl: Parser[FunDecl] = "fun" ~ ident ~ paramList ~ "returns" ~ opt(changeMode) ~ typedIdent ~ opt("global" ~> globImpList) ~ opt("local" ~> cpsDecl) ~ "do" ~ cpsCmd ~ "endfun" ^^ { case "fun" ~ i ~ p ~ "returns" ~ c ~ t ~ il ~ cdecl ~ "do" ~ dcps ~ "endfun" => FunDecl(i, p, c, t, il, cdecl, dcps) }
  def procDecl: Parser[ProcDecl] = "proc" ~ ident ~ paramList ~ opt("global" ~> globImpList) ~ opt("local" ~> cpsDecl) ~ "do" ~ cpsCmd ~ "endproc" ^^ { case "proc" ~ i ~ pl ~ gimp ~ ldecl ~ "do" ~ cmds ~ "endproc" => ProcDecl(i, pl, gimp, ldecl, cmds) }

  // params
  def paramList: Parser[ParamList] = "(" ~ repsep(parameter, ",") ~ ")" ^^ { case "(" ~ p ~ ")" => ParamList(p) }
  def parameter: Parser[Parameter] = opt(flowMode) ~ opt(mechMode) ~ opt(changeMode) ~ typedIdent ^^ { case f ~ m ~ c ~ t => Parameter(f, m, c, t) }
  def typedIdent: Parser[TypedIdent] = ident ~ ":" ~ atomtype ^^ { case i ~ ":" ~ t => TypedIdent(i, t) }

  case class ParseException(n: String) extends CompilerException(n)

  def parse(i: String): Expr = {
    parseAll(expr, i) match {
      case Success(result: Expr, _) => return result
      case failure: NoSuccess => throw ParseException("Parser Error at " + failure.next.pos.line + ":" + failure.next.pos.column + "\n" + failure.next.pos.longString + "\nMsg: " + failure.msg) // "at " + n.pos.line + ":" + n.pos.column + "\n" + n.pos.longString + 
    }
  }
}