package ch.fhnw.parsetable

import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

// http://pages.cpsc.ucalgary.ca/~robin/class/411/LL1.2.html
trait LL1 {

  val nullables = new ListBuffer[NT];
  val followCache = new HashMap[NT, Set[T]];

  def nullable(sl: List[Symbol], prods: List[Production]): Boolean = {
    sl.forall(s => s match {
      case T("") => true
      case t: T => false
      case nt: NT => nullable(nt, prods)
      case _ => throw new IllegalStateException
    })
  }
  def nullable(nt: NT, prods: List[Production]): Boolean = {
    if (nullables.find(n => n.s == nt.s).isDefined) return true;

    if (prods.exists(prod => prod.l == nt && nullable(prod.r, prods))) {
      nullables += nt;
      return true;
    }
    return false;
  }

  def onlyEpsilon(sl: List[Symbol]): Boolean = sl.forall(s => s match { case T("") => true; case _ => false; });

  def first(ls: List[Symbol], prods: List[Production]): Set[T] = {
    ls.foreach(s => s match {
      case T("") => return Set()
      case t: T => return (t :: Nil).toSet
      case nt: NT => return first(nt, prods)
      case _ => throw new IllegalStateException
    })
    Set();
  }
  def first(nt: Symbol, prods: List[Production]): Set[T] = {
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
              if (nullable(nt, prods) && prod.r.tail.headOption.isDefined) firsts ++= (first(nt, prods) ++ first(prod.r.tail.head, prods))
              else firsts ++= first(nt, prods)
            }
            case _ => throw new IllegalStateException;
          }
        }
      }
    })
    return firsts.toSet;
  }

  // TODO chache results
  def follow(nt: NT, prods: List[Production], startSymbol: String = "e"): Set[T] = {
    val cached = followCache.get(nt)
    if (cached.isDefined) return cached.get;

    val follows = new HashSet[T]
    if (nt.s == startSymbol) follows += T("$")

    prods.foreach(prod => {
      headTailZip(prod.r).foreach(t => {
        if (t._1 == nt) {
          if (nullable(t._2, prods) && nt != prod.l) {
            // marked as visited
            followCache += (nt -> Set())

            follows ++= first(t._2, prods) ++ follow(prod.l, prods, startSymbol)
          } else {
            follows ++= first(t._2, prods);
          }
        }
      })
    })

    followCache += (nt -> follows.toSet)
    follows.toSet
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
    if (l.size == 0) Nil
    else (l.head, l.tail) :: headTailZip(l.tail)
}