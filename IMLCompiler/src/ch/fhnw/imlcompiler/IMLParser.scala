package ch.fhnw.imlcompiler

import scala.util.parsing.combinator.RegexParsers
import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST.WhileCmd
import ch.fhnw.imlcompiler.AST.Term3
import ch.fhnw.imlcompiler.AST.MultOpr
import ch.fhnw.imlcompiler.AST.Term3
import ch.fhnw.imlcompiler.AST.TimesOpr
import ch.fhnw.imlcompiler.AST.IntLiteral
import ch.fhnw.imlcompiler.AST.Factor
import ch.fhnw.imlcompiler.AST.Cmd
import ch.fhnw.imlcompiler.AST.Cand
import ch.fhnw.imlcompiler.AST.BoolOpr
import ch.fhnw.imlcompiler.AST.Expr
import ch.fhnw.imlcompiler.AST.Term1
import ch.fhnw.imlcompiler.AST.Term2
import ch.fhnw.imlcompiler.AST.RelOpr
import ch.fhnw.imlcompiler.AST.EQ
import ch.fhnw.imlcompiler.AST.PlusOpr
import ch.fhnw.imlcompiler.AST.AddOpr
import ch.fhnw.imlcompiler.AST.Cor
import ch.fhnw.imlcompiler.AST.BoolLiteral

class IMLParser extends RegexParsers {
  def program: Parser[Program] = "program" ~ whileCmd ~ "endprogram" ^^ { case "program" ~ x2 ~ "endprogram" => Program(x2) }
  def whileCmd: Parser[WhileCmd] = "while" ~ expr ~ "do" ~ "endwhile" ^^ { case "while" ~ t1 ~ "do" ~ "endwhile" => WhileCmd(t1, new Cmd()) }
  def intLiteral: Parser[IntLiteral] = "[0-9]+".r ^^ { x => { IntLiteral(x.toInt) } }
  def boolLiteral: Parser[BoolLiteral] = "[true|false]".r ^^ {x => BoolLiteral(x.toBoolean) }
  def factor: Parser[Factor] = intLiteral | boolLiteral;

  def multOpr: Parser[MultOpr] = "*" ^^^ { TimesOpr() }
  def boolOpr: Parser[BoolOpr] = "[cand|cor]" ^^ { case "cand" => Cand() case "cor" => Cor() }
  def relOpr: Parser[RelOpr] = "==" ^^^ { EQ() }
  def addOpr: Parser[AddOpr] = "+" ^^^ { PlusOpr() }

  def expr: Parser[Expr] = term1 ~ boolOpr ~ term1 ^^ { case x1 ~ x2 ~ x3 => Expr(x1, x2, x3) }
  def term1: Parser[Term1] = term2 ~ relOpr ~ term2 ^^ { case x1 ~ x2 ~ x3 => Term1(x1, x2, x3) }
  def term2: Parser[Term2] = term3 ~ addOpr ~ term3 ^^ { case x1 ~ x2 ~ x3 => Term2(x1, x2, x3) }
  def term3: Parser[Term3] = factor ~ multOpr ~ factor ^^ { case x1 ~ x2 ~ x3 => Term3(x1, x2, x3) }

  def apply(input: String): Program = parseAll(program, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

}