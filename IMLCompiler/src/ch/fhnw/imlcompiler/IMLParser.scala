package ch.fhnw.imlcompiler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import ch.fhnw.imlcompiler.AST._

trait IMLParser extends RegexParsers {
  def program: Parser[Program] = "program" ~ ident ~ whileCmd ~ "endprogram" ^^ { case "program" ~ x1 ~ x2 ~ "endprogram" => Program(x2) }
  def whileCmd: Parser[WhileCmd] = "while" ~ expr ~ "do" ~ "endwhile" ^^ { case "while" ~ t1 ~ "do" ~ "endwhile" => WhileCmd(t1, new Cmd()) }

  def multOpr: Parser[MultOpr] = "*" ^^^ { TimesOpr() } | "div" ^^^ { DivOpr() } | "mod" ^^^ { ModOpr() }
  def boolOpr: Parser[BoolOpr] = "cand" ^^^ { Cand() } | "cor" ^^^ { Cor() }
  def relOpr: Parser[RelOpr] = "==" ^^^ { EQ() } | "!=" ^^^ { NE() } | ">" ^^^ { GT() } | "<" ^^^ { LT() } | "<=" ^^^ { LE() } | ">=" ^^^ { GE() }
  def addOpr: Parser[AddOpr] = "-" ^^^ { MinusOpr() } | "+" ^^^ { PlusOpr() }

  // TODO exclude keywords!!
  def ident: Parser[Ident] = raw"[A-Za-z0-9]+".r ^^ { x => Ident(x.toString()) }

  // literals
  def literal: Parser[Literal] = intLiteral | boolLiteral
  def intLiteral: Parser[IntLiteral] = "[0-9]+".r ^^ { x => { IntLiteral(x.toInt) } }
  def boolLiteral: Parser[BoolLiteral] = ("true" | "false") ^^ { x => BoolLiteral(x.toBoolean) }

  def monadicExpr: Parser[MonadicExpr] = monadicAddExpr;
  def monadicAddExpr: Parser[MonadicAddExpr] = addOpr ~ factor ^^ { case x1 ~ x2 => MonadicAddExpr(x1, x2) }

  // factor
  def factor: Parser[Expr] = literal | ident | monadicExpr | "(" ~> expr <~ ")"

  def expr: Parser[Expr] = term1 * (boolOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) })
  def term1: Parser[Expr] = term2 ~ relOpr ~ term2 ^^ { case x1 ~ o ~ x2 => DyadicExpr(x1, o, x2) } | term2 ^^ { case term2 => term2 }
  def term2: Parser[Expr] = term3 * (addOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) })
  def term3: Parser[Expr] = factor * (multOpr ^^ { case op => DyadicExpr(_: Expr, op, _: Expr) })

  def flowMode: Parser[FlowMode] = "in" ^^^ { In() } | "out" ^^^ { Out() } | "inout" ^^^ { InOut() }
  def changeMode: Parser[ChangeMode] = "var" ^^^ { Var() } | "const" ^^^ { Const() }

  def globImpList: Parser[GlobImpList] = repsep(globImport, ",") ^^ { case g => GlobImpList(g)}
  def globImport: Parser[GlobImport] = flowMode ~ changeMode ~ ident ^^ { case f ~ c ~ i => GlobImport(f, c, i) }
}