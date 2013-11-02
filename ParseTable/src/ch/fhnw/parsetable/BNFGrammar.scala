package ch.fhnw.parsetable

object BNFGrammar {
  case class Grammar(prods:List[Production]);
  case class Production(l: NT, r: List[Symbol])
  abstract sealed class Symbol;
  case class T(s: String) extends Symbol;
  case class NT(s: String) extends Symbol;
  case class Rep(r: List[Symbol]) extends Symbol
  case class Opt(o: List[Symbol]) extends Symbol
  case class Alt(a: List[List[Symbol]]) extends Symbol
}