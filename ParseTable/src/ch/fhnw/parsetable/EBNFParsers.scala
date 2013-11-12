package ch.fhnw.parsetable

import scala.util.parsing.combinator.RegexParsers
import ch.fhnw.parsetable.BNFGrammar._

trait EBNFParsers extends RegexParsers {
  def grammar: Parser[Grammar] = repsep(production, ";") ^^ { case prods => Grammar(prods) }
  def production: Parser[Production] = positioned(nonTerminal ~ "::=" ~ term0 ^^ { case nt ~ "::=" ~ s => Production(nt, s) })
  def terminal: Parser[T] = positioned("[A-Z]+".r.withFailureMessage("terminal symbol expected") ^^ { case t => T(t) })
  def nonTerminal: Parser[NT] = positioned("[a-z][a-zA-Z0-9]*".r.withFailureMessage("non terminal symbol expected") ^^ { case nt => NT(nt) })

  def term0: Parser[List[Symbol]] = repsep(rep(term1), "|") ^^ { case x => if (x.size > 1) List(Alt(x)) else x.head }
  def term1: Parser[Symbol] = positioned(repTerm | optTerm | symbol)
  def repTerm: Parser[Symbol] = positioned("{" ~ term0 ~ "}" ^^ { case "{" ~ rep ~ "}" => Rep(rep) })
  def optTerm: Parser[Symbol] = positioned("[" ~ term0 ~ "]" ^^ { case "[" ~ rep ~ "]" => Opt(rep) })
  def symbol: Parser[Symbol] = positioned(terminal | nonTerminal)

  def parse(i: String): Grammar = {
    parseAll(grammar, i) match {
      case Success(result: Grammar, _) => return result
      case failure: NoSuccess => throw PraseTableException("Parser Error at " + failure.next.pos.line + ":" + failure.next.pos.column + "\n" + failure.next.pos.longString + "\nMessage: " + failure.msg);
    }
  }

  
}