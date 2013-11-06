package ch.fhnw.parsetable
import scala.collection.convert.WrapAsJava
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object BNFGrammar extends WrapAsJava {
  case class Grammar(prods: List[Production]);
  case class Production(l: NT, r: List[Symbol]) { def r2(): java.util.List[Symbol] = r }
  abstract sealed class Symbol { def valueString = "" };
  case class T(s: String) extends Symbol { override def valueString = if (s.size > 0) s else "espilon" };
  case class NT(s: String) extends Symbol { override def valueString = s };
  case class Rep(r: List[Symbol]) extends Symbol
  case class Opt(o: List[Symbol]) extends Symbol
  case class Alt(a: List[List[Symbol]]) extends Symbol

  // TODO check undefined not via this but in a seperate pass
  def checkGrammar(g: Grammar, root: Production): Map[NT, (Boolean, Boolean)] = {
    val undefined = ListBuffer[NT]();
    val reachableMap = new HashMap[NT, Boolean]
    val nts = g.prods.collect({ case p => p.l }).distinct

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

  // TODO this is not quite right, rather try to find a path from the root to each production (to find out if unreachable)
  def checkGrammar(g: Grammar): HashMap[NT, (Boolean, Boolean)] = {
    // check for possible unreachable or undefined non terminals
    val unreachable = HashMap[NT, (Boolean, Boolean)]()
    g.prods.foreach(p => {
      val t = unreachable.get(p.l);
      t match {
        case None => unreachable.put(p.l, (true, false))
        case Some(s) => unreachable.put(p.l, (true, s._2))
      }
      checkSymbol(p.r, unreachable)
    })

    unreachable;
  }

  def checkSymbol(ls: List[Symbol], unreachable: HashMap[NT, (Boolean, Boolean)]): Unit = {
    ls.foreach(s => {
      s match {
        case nt: NT => {
          val t = unreachable.get(nt);
          t match {
            case None => unreachable.put(nt, (false, true))
            case Some(s) => unreachable.put(nt, (s._1, true))
          }
        }
        case t: T =>
        case opt: Opt => checkSymbol(opt.o, unreachable)
        case rep: Rep => checkSymbol(rep.r, unreachable)
        case alt: Alt => alt.a.foreach(ls2 => checkSymbol(ls2, unreachable));
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
      case _ => print("ERROR");
    }
  }

  case class PraseTableException(v: String) extends RuntimeException(v)
}