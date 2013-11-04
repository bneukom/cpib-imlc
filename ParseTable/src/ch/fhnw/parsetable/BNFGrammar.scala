package ch.fhnw.parsetable

object BNFGrammar {
  case class Grammar(prods: List[Production]);
  case class Production(l: NT, r: List[Symbol])
  abstract sealed class Symbol { def valueString = "" };
  case class T(s: String) extends Symbol { override def valueString = if (s.size > 0) s else "espilon" };
  case class NT(s: String) extends Symbol { override def valueString = s };
  case class Rep(r: List[Symbol]) extends Symbol
  case class Opt(o: List[Symbol]) extends Symbol
  case class Alt(a: List[List[Symbol]]) extends Symbol

  def printGrammar(g: Grammar) = g.prods.foreach(prod => printProduction(prod))

  def printProduction(p: Production) = {
    printSymbol(p.l);
    print(" ::= ");
    printSymbols(p.r);
    println();
  }

  def printSymbols(l: List[Symbol]) = l.foreach(s => { printSymbol(s); print(" ") })

  def printSymbol(s: Symbol) = {
    s match {
      case nt: NT => print(nt.s)
      case nt: T => print(nt.s)
      case _ => print("ERROR");
    }
  }

  case class PraseTableException(v: String) extends RuntimeException(v)
}