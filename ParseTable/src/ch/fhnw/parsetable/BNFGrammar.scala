package ch.fhnw.parsetable
import scala.collection.convert.WrapAsJava
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional

object BNFGrammar extends WrapAsJava {
  abstract sealed class GrammarRoot extends Positional
  case class Grammar(prods: List[Production]) extends GrammarRoot;
  case class Production(l: NT, r: List[Symbol]) extends GrammarRoot { def r2(): java.util.List[Symbol] = r }
  abstract sealed class Symbol extends GrammarRoot { def valueString = "" };
  case class T(s: String) extends Symbol { override def valueString = if (s.size > 0) s else "espilon" };
  case class NT(s: String) extends Symbol { override def valueString = s };
  case class Rep(r: List[Symbol]) extends Symbol
  case class Opt(o: List[Symbol]) extends Symbol
  case class Alt(a: List[List[Symbol]]) extends Symbol

  def checkGrammar(g: Grammar, root: Production): Map[NT, (Boolean, Boolean)] = {
    val undefined = ListBuffer[NT]();
    val reachableMap = new HashMap[NT, Boolean]
    val nts = g.prods.tail.collect({ case p => p.l }).distinct

    // check if all nts are reachable from the root production
    nts.foreach(nt => reachableMap += (nt -> reachable(nt, root.r, g.prods, undefined, ListBuffer())));

    val result = new HashMap[NT, (Boolean, Boolean)]
    reachableMap.foreach(nt => result += (nt._1 -> (nt._2, !undefined.find(x => x eq nt._1).isDefined)))

    result.toMap
  }

  def reachable(toFind: NT, ls: List[Symbol], productions: List[Production], undefined: ListBuffer[NT], visited: ListBuffer[List[Symbol]]): Boolean = {
    if (visited.contains(ls)) return false;

    visited += ls;

    ls.exists(s => {
      s match {
        case nt: NT => {
          if (nt == toFind) true
          else {
            val prod = productions.find(f => f.l == nt)
            prod match {
              case None => undefined += nt; false;
              case Some(x) => reachable(toFind, x.r, productions, undefined, visited)
            }
          }
        }
        case t: T => false
        case opt: Opt => reachable(toFind, opt.o, productions, undefined, visited)
        case rep: Rep => reachable(toFind, rep.r, productions, undefined, visited)
        case alt: Alt => alt.a.exists(ls2 => reachable(toFind, ls2, productions, undefined, visited));
      }
    })
  }

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
      case _ => print("ERROR"); // TODO implement printing for EBNF grammars
    }
  }

  case class PraseTableException(v: String) extends RuntimeException(v)
}