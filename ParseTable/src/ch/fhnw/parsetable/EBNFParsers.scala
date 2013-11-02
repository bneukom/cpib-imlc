package ch.fhnw.parsetable

import scala.util.parsing.combinator.RegexParsers
import ch.fhnw.parsetable.BNFGrammar._

trait EBNFParsers extends RegexParsers {
  def grammar: Parser[Grammar] = repsep(production, ";") ^^ { case prods => Grammar(prods) }
  def production: Parser[Production] = nonTerminal ~ "::=" ~ term0 ^^ { case nt ~ "::=" ~ s => Production(nt, s) }
  def terminal: Parser[T] = "[A-Z]+".r.withFailureMessage("terminal symbol expected") ^^ { case t => T(t) }
  def nonTerminal: Parser[NT] = "[a-z][a-zA-Z0-9]*".r.withFailureMessage("non terminal symbol expected") ^^ { case nt => NT(nt) }

  def term0: Parser[List[Symbol]] = repsep(rep(term1), "|") ^^ { case x => if (x.size > 1) List(Alt(x)) else x.head }
  def term1: Parser[Symbol] = repTerm | optTerm | symbol
  def repTerm: Parser[Symbol] = "{" ~ term0 ~ "}" ^^ { case "{" ~ rep ~ "}" => Rep(rep) }
  def optTerm: Parser[Symbol] = "[" ~ term0 ~ "]" ^^ { case "[" ~ rep ~ "]" => Opt(rep) }
  def symbol: Parser[Symbol] = terminal | nonTerminal

  def parse(i: String): Grammar = {
    parseAll(grammar, i) match {
      case Success(result: Grammar, _) => return result
      case failure: NoSuccess => throw PraseTableException(failure.msg);
    }
  }

  case class PraseTableException(v:String) extends RuntimeException(v)
}