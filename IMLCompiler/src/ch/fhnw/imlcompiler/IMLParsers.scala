package ch.fhnw.imlcompiler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import ch.fhnw.imlcompiler.AST._

// TODO positioned for all parsers needed
trait IMLParsers extends RegexParsers {
  def program: Parser[Program] = positioned("program" ~ ident ~ progParamList ~ opt("global" ~> cpsDecl) ~ "do" ~ cpsCmd ~ "endprogram" ^^ { case "program" ~ id ~ progParamList ~ cpsdecl ~ "do" ~ cmd ~ "endprogram" => Program(progParamList, cpsdecl.getOrElse(Nil), cmd) })

  def atomtype: Parser[Type] = "int" ^^^ { IntType } | "bool" ^^^ { BoolType }

  // commands
  def cmd: Parser[Cmd] = positioned(skipCmd | becomesCmd | ifCmd | whileCmd | callCmd | inputCmd | outputCmd)
  def skipCmd: Parser[SkipCmd] = positioned("skip" ^^^ { SkipCmd() })
  def becomesCmd: Parser[BecomesCmd] = positioned(expr ~ ":=" ~ expr ^^ { case lhs ~ ":=" ~ rhs => BecomesCmd(lhs, rhs) })
  def ifCmd: Parser[IfCmd] = positioned("if" ~ expr ~ "do" ~ cpsCmd ~ "else" ~ cpsCmd ~ "endif" ^^ { case "if" ~ expr ~ "do" ~ c1 ~ "else" ~ c2 ~ "endif" => IfCmd(expr, c1, c2) })
  def whileCmd: Parser[WhileCmd] = positioned("while" ~ expr ~ "do" ~ cpsCmd ~ "endwhile" ^^ { case "while" ~ t1 ~ "do" ~ c ~ "endwhile" => WhileCmd(t1, c) })
  // TODO implement globInits
  def callCmd: Parser[CallCmd] = positioned("call" ~ ident ~ tupleExpr ^^ { case "call" ~ i ~ t => CallCmd(i, t) })
  def inputCmd: Parser[InputCmd] = positioned("input" ~> expr ^^ { case e => InputCmd(e) })
  def outputCmd: Parser[OutputCmd] = positioned("output" ~> expr ^^ { case e => OutputCmd(e) })

  def cpsCmd: Parser[List[Cmd]] = repsep(cmd, ";")

  // operators
  def multOpr: Parser[MultOpr] = "*" ^^^ { TimesOpr } | "div" ^^^ { DivOpr } | "mod" ^^^ { ModOpr }
  def boolOpr: Parser[BoolOpr] = "&&" ^^^ { Cand } | "||" ^^^ { Cor }
  def relOpr: Parser[RelOpr] = "==" ^^^ { EQ } | "/=" ^^^ { NE } | "<=" ^^^ { LE } | ">=" ^^^ { GE } | ">" ^^^ { GT } | "<" ^^^ { LT }
  def addOpr: Parser[AddOpr] = "-" ^^^ { MinusOpr } | "+" ^^^ { PlusOpr }

  // TODO change regex for identifier
  def ident: Parser[Ident] = positioned(raw"[A-Za-z]+[A-Za-z0-9]*".r.withFilter(!AST.keywords.contains(_)).withFailureMessage("identifier expected") ^^ { x => Ident(x.toString()) })

  // literals
  def literal: Parser[Literal] = positioned(intLiteral | boolLiteral)
  def intLiteral: Parser[IntLiteral] = positioned("[0-9]+".r ^^ { x => { IntLiteral(x.toInt) } })
  def boolLiteral: Parser[BoolLiteral] = positioned(("true" | "false") ^^ { x => BoolLiteral(x.toBoolean) })
  def notParser: Parser[BoolOpr] = "not" ^^^ { Not }

  // expressions
  def monadicExpr: Parser[MonadicExpr] = positioned(monadicAddExpr | monadicNotExpr)
  def monadicAddExpr: Parser[MonadicExpr] = positioned(addOpr ~ factor ^^ { case op ~ exp => MonadicExpr(exp, op) })
  def monadicNotExpr: Parser[MonadicExpr] = positioned(notParser ~ factor ^^ { case n ~ exp => MonadicExpr(exp, n) })

  def factor: Parser[Expr] = positioned(literal ^^ { LiteralExpr(_) } | ident ~ tupleExpr ^^ { case i ~ r => FunCallExpr(i, r) } | ident ~ "init" ^^ { case i ~ "init" => StoreExpr(i, true) } | ident ^^ { case i => StoreExpr(i, false) } | monadicExpr | "(" ~> expr <~ ")")

  def expr: Parser[Expr] = positioned(term1 * (boolOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) }))
  def term1: Parser[Expr] = positioned(term2 ~ relOpr ~ term2 ^^ { case x1 ~ o ~ x2 => DyadicExpr(x1, o, x2) } | term2 ^^ { case term2 => term2 })
  def term2: Parser[Expr] = positioned(term3 * (addOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) }))
  def term3: Parser[Expr] = positioned(factor * (multOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) }))
  def tupleExpr: Parser[TupleExpr] = positioned("(" ~ repsep(expr, ",") ~ ")" ^^ { case "(" ~ e ~ ")" => TupleExpr(e) })

  def flowMode: Parser[FlowMode] = positioned("in" ^^^ { In } | "out" ^^^ { Out } | "inout" ^^^ { InOut }).withFailureMessage("flowmode expected")
  def changeMode: Parser[ChangeMode] = positioned("var" ^^^ { Var } | "const" ^^^ { Const }).withFailureMessage("changemode expected")
  def mechMode: Parser[MechMode] = positioned("ref" ^^^ { Ref } | "copy" ^^^ { Copy }).withFailureMessage("mechmode expected")

  // decls
  def globImpList: Parser[List[GlobImport]] = repsep(globImport, ",")
  def globImport: Parser[GlobImport] = positioned(flowMode ~ changeMode ~ ident ^^ { case f ~ c ~ i => GlobImport(f, c, i) })
  def cpsDecl: Parser[List[Decl]] = repsep(decl, ";")
  def cpsStoreDecl: Parser[List[StoreDecl]] = repsep(stoDecl, ";")
  def decl: Parser[Decl] = positioned(stoDecl | funDecl | procDecl)
  def stoDecl: Parser[StoreDecl] = positioned(changeMode ~ typedIdent ^^ { case c ~ i => StoreDecl(c, i) })
  def funDecl: Parser[FunDecl] = positioned("fun" ~ ident ~ paramList ~ "returns" ~ stoDecl ~ opt("global" ~> globImpList) ~ opt("local" ~> cpsStoreDecl) ~ "do" ~ cpsCmd ~ "endfun" ^^ { case "fun" ~ i ~ p ~ "returns" ~ stodecl ~ globImportList ~ cdecl ~ "do" ~ dcps ~ "endfun" => FunDecl(i, p, stodecl, globImportList.getOrElse(Nil), cdecl.getOrElse(Nil), dcps) })
  def procDecl: Parser[ProcDecl] = positioned("proc" ~ ident ~ paramList ~ opt("global" ~> globImpList) ~ opt("local" ~> cpsStoreDecl) ~ "do" ~ cpsCmd ~ "endproc" ^^ { case "proc" ~ i ~ pl ~ gimp ~ ldecl ~ "do" ~ cmds ~ "endproc" => ProcDecl(i, pl, gimp.getOrElse(Nil), ldecl.getOrElse(Nil), cmds) })

  // params
  def paramList: Parser[List[Parameter]] = "(" ~> repsep(parameter, ",") <~ ")"
  def parameter: Parser[Parameter] = positioned(opt(flowMode) ~ opt(mechMode) ~ opt(changeMode) ~ typedIdent ^^ { case f ~ m ~ c ~ t => Parameter(f, m, c, t) })
  def typedIdent: Parser[TypedIdent] = positioned(ident ~ ":" ~ atomtype ^^ { case i ~ ":" ~ t => TypedIdent(i, t) })

  def progParamList: Parser[List[ProgParameter]] = "(" ~> repsep(progParameter, ",") <~ ")"
  def progParameter: Parser[ProgParameter] = positioned(opt(flowMode) ~ opt(changeMode) ~ typedIdent ^^ { case f ~ c ~ t => ProgParameter(f, c, t) })

  case class ParseException(n: String) extends CompilerException(n)

  def parse(i: String): Program = {
    parseAll(program, i) match {
      case Success(result: Program, _) => return result
      case failure: NoSuccess => throw ParseException("Parser Error at " + failure.next.pos.line + ":" + failure.next.pos.column + "\n" + failure.next.pos.longString + "\nMessage: " + failure.msg) // "at " + n.pos.line + ":" + n.pos.column + "\n" + n.pos.longString + 
    }
  }

}