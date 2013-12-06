package ch.fhnw.parsetable

import scala.collection.immutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import ch.fhnw.parsetable.BNFGrammar.NT
import ch.fhnw.parsetable.BNFGrammar.Production
import ch.fhnw.parsetable.BNFGrammar.Symbol
import ch.fhnw.parsetable.BNFGrammar.T
import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.immutable.List

// http://pages.cpsc.ucalgary.ca/~robin/class/411/LL1.2.html
trait LL1 {

  // TODO use map for nullable too (map NT -> bool)
  val nullables = new ListBuffer[NT];
  val followCache = new HashMap[NT, Set[T]];

  def nullable(sl: List[Symbol], prods: List[Production], visited: ListBuffer[NT]): Boolean = {
    sl.forall(s => s match {
      case T("") => true
      case t: T => false
      case nt: NT => nullable(nt, prods, visited)
      case _ => throw new IllegalStateException
    })
  }

  def nullable(nt: NT, prods: List[Production], visited: ListBuffer[NT]): Boolean = {

    // if has already been visited check if is in cache otherwise return false (prohibit possible endless loop)
    if (visited.contains(nt)) return nullables.find(n => n.s == nt.s).isDefined;
    visited += nt;

    if (nullables.find(n => n.s == nt.s).isDefined) return true;

    if (prods.exists(prod => prod.l == nt && nullable(prod.r, prods, visited))) {
      nullables += nt;
      return true;
    }
    return false;
  }

  def onlyEpsilon(sl: List[Symbol]): Boolean = sl.forall(s => s match { case T("") => true; case _ => false; });

  def first(ls: List[Symbol], prods: List[Production], visited: ListBuffer[Symbol]): Set[T] = {
    if (ls.isEmpty || onlyEpsilon(ls)) return Set()

    ls.head match {
      case t: T => return Set(t);
      case nt: NT => return if (nullable(ls.head :: Nil, prods, ListBuffer[NT]())) first(ls.head, prods, visited) ++ first(ls.tail, prods, visited) else first(ls.head, prods, visited)
      case _ => throw new IllegalStateException
    }

    Set()
  }

  def first(nt: Symbol, prods: List[Production], visited: ListBuffer[Symbol] = ListBuffer[Symbol]()): Set[T] = {
    if (visited.contains(nt)) return Set()
    visited += nt

    return prods.foldRight(Set[T]())((prod, firsts) => if (prod.l == nt) firsts ++ first(prod.r, prods, visited) else firsts);
  }

  def follow(nt: NT, prods: List[Production], startSymbol: String, visited: ListBuffer[Symbol] = ListBuffer[Symbol](), cache: Boolean = true): Set[T] = {
    if (visited.contains(nt)) return Set()
    visited += nt

    val cached = followCache.get(nt)
    if (cached.isDefined) return cached.get;

    val follows = new HashSet[T]
    if (nt.s == startSymbol) follows += T("$")

    val g = Grammar(List(Production(NT("a"), List(T("b")))));

    prods.foreach(prod => {
      val headTail = headTailZip(prod.r);
      headTail.foreach(t => {
        if (t._1 == nt) {
          if (nullable(t._2, prods, ListBuffer[NT]()) && nt != prod.l) {
            val f = first(t._2, prods, ListBuffer[Symbol]())
            val fol = follow(prod.l, prods, startSymbol, visited, false); // do NOT cache due to possible invalid results when in circles
            follows ++= (f ++ fol)
          } else {
            follows ++= first(t._2, prods, ListBuffer[Symbol]());
          }
        }
      })
    })

    if (cache) followCache += (nt -> follows.toSet)
    follows.toSet
  }

  def ts(prods: List[Production]): Set[T] = {
    val ts = new HashSet[T];
    prods.foreach(prod => prod.r.foreach(s => s match { case t: T => if (t.s.size > 0) ts += t case _ => }))
    ts.toSet
  }

  def nts(prods: List[Production]): Set[NT] = {
    val nts = new HashSet[NT];
    prods.foreach(prod => {
      nts += prod.l;
      prod.r.foreach(s => s match { case nt: NT => nts += nt case _ => })
    })
    nts.toSet
  }

  def headTailZip[A](l: List[A]): List[(A, List[A])] = {
    if (l.isEmpty) Nil
    else (l.head, l.tail) :: headTailZip(l.tail)
  }

  def genParseTable(productions: List[Production], startSymbol: String): ParseTable = {
    val terminals = ts(productions).toList :+ T("$");
    val nonTerminals = nts(productions).toList;
    val table = Array.ofDim[List[Production]](nonTerminals.size, terminals.size)

    // TODO first fill with empty lists;

    productions.foreach(prod => {
      // rule 1
      val firsts = first(prod.r, productions, ListBuffer[Symbol]());
      firsts.foreach(f => {
        fillTable(nonTerminals, terminals, table, f, prod.l, prod)
      })

      // rule 2
      if (nullable(prod.r, productions, ListBuffer[NT]())) {
        val follows = follow(prod.l, productions, startSymbol)
        follows.foreach(f => {
          fillTable(nonTerminals, terminals, table, f, prod.l, Production(prod.l, T("") :: Nil))
        })
      }
    })

    new ParseTable(terminals, nonTerminals, table)
  }

  private def fillTable(nonTerminals: List[NT], terminals: List[T], table: Array[Array[List[Production]]], t: T, nt: NT, p: Production): Unit = {
    val nonTerminalIndex = nonTerminals.indexOf(nt)
    val terminalIndex = terminals.indexOf(t);
    if (table(nonTerminalIndex)(terminalIndex) == null) {
      table(nonTerminalIndex)(terminalIndex) = List(p);
    } else {
      table(nonTerminalIndex)(terminalIndex) = p :: table(nonTerminalIndex)(terminalIndex);
      System.err.println("Grammar is not ll(1) (duplicate parse table entry)")
    }
  }

}