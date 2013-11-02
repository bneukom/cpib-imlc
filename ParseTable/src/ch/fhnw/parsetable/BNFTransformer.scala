package ch.fhnw.parsetable

import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import scala.collection.TraversableLike
import scala.collection.Traversable

trait BNFTransformer {

  val newProds: MutableList[Production] = new MutableList[Production]

  def transform(ebnf: Grammar): Grammar = {
    Grammar(transformProds(ebnf.prods))
  }

  def transformProds(prods: List[Production]): List[Production] = {

    val newProds = new MutableList[Production]

    prods.foreach(p => {
      newProds ++= transformProd(p);
    })

    newProds.toList
  }

  def transformProd(prod: Production): List[Production] = {
    val prods = new ListBuffer[Production]
    val symbols = new ListBuffer[Symbol]

    // transform each rhs symbol of the production
    prod.r.foreach(s => {
      symbols += transformSymbol(s, prods);
    })

    // return a list of the new production (while transforming the symbols, new productions might have been added)
    Production(prod.l, symbols.toList) :: prods.toList
  }

  def transformSymbol(s: Symbol, prods: ListBuffer[Production]): Symbol = {
    s match {
      case Opt(l) => {
        val optNt = NT("opt" + concat(l))
        prods += Production(optNt, List(Alt(List(List(T("")), l))))
        return optNt;
      } // TODO create new productions and new NT for these productions
      case t: T => t
      case nt: NT => nt
    }
  }

  def concat(l: Traversable[Any]): String = {
    var s = "";
    l.foreach(o => o match { case l2: Traversable[Any] => s += concat(l2) case nt: NT => s += nt.s case t: T => s += t.s })
    return s;
  }

}