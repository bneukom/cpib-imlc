package ch.fhnw.imlcompiler

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import ch.fhnw.imlcompiler.AST._

class IMLParser extends RegexParsers with PackratParsers {
  def program: Parser[Program] = "program" ~ whileCmd ~ "endprogram" ^^ { case "program" ~ x2 ~ "endprogram" => Program(x2) }
  def whileCmd: Parser[WhileCmd] = "while" ~ expr ~ "do" ~ "endwhile" ^^ { case "while" ~ t1 ~ "do" ~ "endwhile" => WhileCmd(t1, new Cmd()) }

  def multOpr: Parser[MultOpr] = "*" ^^^ { TimesOpr() }
  def boolOpr: Parser[BoolOpr] = ("cand" | "cor") ^^ { x => x match { case "cand" => Cand() case "cor" => Cor() } }
  def relOpr: Parser[RelOpr] = "==" ^^^ { EQ() }
  def addOpr: Parser[AddOpr] = "+" ^^^ { PlusOpr() }

  def intLiteral: Parser[IntLiteral] = "[0-9]+".r ^^ { x => { IntLiteral(x.toInt) } }
  def boolLiteral: Parser[BoolLiteral] = ("true" | "false") ^^ { x => BoolLiteral(x.toBoolean) }
  //  def boolLiteral: Parser[BoolLiteral] = "[true|false]".r ^^ { x => BoolLiteral(x.toBoolean) }
  def factor: Parser[Factor] = intLiteral | boolLiteral;

  // parser combinator order does matter (term1, term2, term3 from lederer -> operator precedence)
  lazy val expr: PackratParser[Expr] = dyadicExpr | parExpr | factor // identifier?
  lazy val parExpr: PackratParser[Expr] = "(" ~> expr <~ ")"
  lazy val dyadicExpr: PackratParser[DyadicExpr] = multExpr | relExpr | addExpr
  lazy val multExpr: PackratParser[MultExpr] = expr ~ multOpr ~ expr ^^ { case x1 ~ x2 ~ x3 => MultExpr(x1, x2, x3) }
  lazy val addExpr: PackratParser[AddExpr] = expr ~ addOpr ~ expr ^^ { case x1 ~ x2 ~ x3 => AddExpr(x1, x2, x3) }
  lazy val relExpr: PackratParser[RelExpr] = expr ~ relOpr ~ expr ^^ { case x1 ~ x2 ~ x3 => RelExpr(x1, x2, x3) }

  def apply(input: String): Expr = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

}