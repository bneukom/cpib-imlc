package ch.fhnw.imlcompiler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import ch.fhnw.imlcompiler.AST._

class IMLParser extends RegexParsers with PackratParsers {
  def program: Parser[Program] = "program" ~ ident ~ whileCmd ~ "endprogram" ^^ { case "program" ~ x1 ~ x2 ~ "endprogram" => Program(x2) }
  def whileCmd: Parser[WhileCmd] = "while" ~ expr ~ "do" ~ "endwhile" ^^ { case "while" ~ t1 ~ "do" ~ "endwhile" => WhileCmd(t1, new Cmd()) }

  def multOpr: Parser[MultOpr] = "*" ^^^ { TimesOpr() } | "div" ^^^ { DivOpr() }  | "mod" ^^^ { ModOpr() }
  def boolOpr: Parser[BoolOpr] = "cand" ^^^ { Cand() } | "cor" ^^^ { Cor() }
  def relOpr: Parser[RelOpr] = "==" ^^^ { EQ() } | "!=" ^^^ { NE() }
  def addOpr: Parser[AddOpr] = "-" ^^^ { MinusOpr() } | "+" ^^^ { PlusOpr() }

  // TODO exclude keywords!!
  def ident: Parser[Ident] = raw"[A-Za-z0-9]+".r ^^ { x => Ident(x.toString()) }

  def intLiteral: Parser[IntLiteral] = "[0-9]+".r ^^ { x => { IntLiteral(x.toInt) } }
  def boolLiteral: Parser[BoolLiteral] = ("true" | "false") ^^ { x => BoolLiteral(x.toBoolean) }
  def parExpr: Parser[Expr] = "(" ~> expr <~ ")"
  def factor: Parser[Expr] = intLiteral | boolLiteral | ident | parExpr | monadicExpr;

  // parser combinator order does matter (term1, term2, term3 from lederer -> operator precedence)
  // packrat parser because of left recursion
  // TODO packrat parser really needed?
  lazy val expr: PackratParser[Expr] = dyadicExpr | monadicExpr | parExpr | factor // identifier?
  def dyadicExpr: Parser[DyadicExpr] = multExpr | addExpr | relExpr
  def multExpr: Parser[MultExpr] = expr ~ multOpr ~ factor ^^ { case x1 ~ x2 ~ x3 => MultExpr(x1, x2, x3) }
  def addExpr: Parser[DyadicExpr] = expr ~ addOpr ~ factor ^^ { case x1 ~ x2 ~ x3 => DyadicAddExpr(x1, x2, x3) }
  def relExpr: Parser[RelExpr] = expr ~ relOpr ~ expr ^^ { case x1 ~ x2 ~ x3 => RelExpr(x1, x2, x3) }
  def monadicExpr: Parser[MonadicExpr] = monadicAddExpr;
  def monadicAddExpr: Parser[MonadicAddExpr] = addOpr ~ factor ^^ { case x1 ~ x2 => MonadicAddExpr(x1, x2) }

  def apply(input: String): Program = parseAll(program, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

}