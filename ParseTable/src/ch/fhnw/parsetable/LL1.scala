package ch.fhnw.parsetable

import scala.collection.immutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

import ch.fhnw.parsetable.BNFGrammar.NT
import ch.fhnw.parsetable.BNFGrammar.Production
import ch.fhnw.parsetable.BNFGrammar.Symbol
import ch.fhnw.parsetable.BNFGrammar.T

// http://pages.cpsc.ucalgary.ca/~robin/class/411/LL1.2.html
trait LL1 {

  // TODO use map for nullable too (map NT -> bool)
  val nullables = new ListBuffer[NT];
  val followCache = new HashMap[NT, Set[T]];

  // possible endless recursion x ::= y; y ::= x
  def nullable(sl: List[Symbol], prods: List[Production], visited: ListBuffer[NT]): Boolean = {
    sl.forall(s => s match {
      case T("") => true
      case t: T => false
      case nt: NT => nullable(nt, prods, visited)
      case _ => throw new IllegalStateException
    })
  }

  def nullable(nt: NT, prods: List[Production], visited: ListBuffer[NT]): Boolean = {
    if (visited.contains(nt)) return false;

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
    ls.foreach(s => s match {
      case T("") => return Set()
      case t: T => return (t :: Nil).toSet
      case nt: NT => return first(nt, prods, visited)
      case _ => throw new IllegalStateException
    })
    Set();
  }
  def first(nt: Symbol, prods: List[Production], visited: ListBuffer[Symbol] = ListBuffer[Symbol]()): Set[T] = {
    if (visited.contains(nt)) return Set()
    visited += nt

    val firsts = new HashSet[T]
    prods.foreach(prod => {
      if (prod.l == nt) {
        if (onlyEpsilon(prod.r))
          firsts ++= Set[T]();
        else {
          prod.r.head match {
            case T("") => {}; // empty
            case t: T => firsts ++= (t :: Nil).toSet
            case nt: NT => {
              if (nullable(nt, prods, ListBuffer[NT]()) && prod.r.tail.headOption.isDefined) firsts ++= (first(nt, prods, visited) ++ first(prod.r.tail.head, prods, visited))
              else firsts ++= first(nt, prods, visited)
            }
            case _ => throw new IllegalStateException;
          }
        }
      }
    })
    return firsts.toSet;
  }

  def follow(nt: NT, prods: List[Production], startSymbol: String, visited: ListBuffer[Symbol] = ListBuffer[Symbol](), cache: Boolean = true): Set[T] = {
    if (visited.contains(nt)) return Set()
    visited += nt

    val cached = followCache.get(nt)
    if (cached.isDefined) return cached.get;

    val follows = new HashSet[T]
    if (nt.s == startSymbol) follows += T("$")

    prods.foreach(prod => {
      val headTail = headTailZip(prod.r);
      headTail.foreach(t => {
        if (t._1 == nt) {
          if (nullable(t._2, prods, ListBuffer[NT]()) && nt != prod.l) {
            val f = first(t._2, prods, ListBuffer[Symbol]())
            val fol = follow(prod.l, prods, startSymbol, visited, false); // no NOT cache due to possible invalid results when in circles
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

  def headTailZip[A](l: List[A]): List[(A, List[A])] =
    if (l.isEmpty) Nil
    else (l.head, l.tail) :: headTailZip(l.tail)

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

  // TODO use List[Production]
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