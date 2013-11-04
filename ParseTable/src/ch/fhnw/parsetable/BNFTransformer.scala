package ch.fhnw.parsetable

import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer
import scala.collection.TraversableLike
import scala.collection.Traversable

trait BNFTransformer {

  val newLhsNonTerminal: MutableList[NT] = new MutableList[NT]

  // TODO remember every T and NT for parse table generating

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
      symbols += transformSymbol(prod.l, s, prods);
    })

    // return a list of the new production (while transforming the symbols, new productions might have been added)
    Production(prod.l, symbols.toList) :: prods.toList
  }

  def transformSymbol(lhs: NT, s: Symbol, prods: ListBuffer[Production]): Symbol = {
    s match {
      case Opt(l) => {
        //        val optNt = NT("opt" + concat(l) + "_" + newLhsNonTerminal.count(nt => nt.s.startsWith("opt" + concat(l))))
        val optNt = NT("opt" + concat(l) + "_" + newLhsNonTerminal.count(nt => nt.s matches "opt" + concat(l) + "_[0-9]*"))
        newLhsNonTerminal += optNt;

        val internalSymbols = new ListBuffer[Symbol]
        l.foreach(s2 => internalSymbols += transformSymbol(lhs, s2, prods));
        prods += Production(optNt, internalSymbols.toList);
        prods += Production(optNt, List(T("")))
        return optNt;
      }
      case Rep(l) => {
        val repNt = NT("rep" + concat(l) + "_" + newLhsNonTerminal.count(nt => nt.s.startsWith("rep" + concat(l))))
        newLhsNonTerminal += repNt;

        val internalSymbols = new ListBuffer[Symbol]
        l.foreach(s2 => internalSymbols += transformSymbol(lhs, s2, prods));
        prods += Production(repNt, internalSymbols.toList :+ repNt);
        prods += Production(repNt, List(T("")))
        return repNt;
      }
      case Alt(ll) => {
        val altNT = NT("alt" + lhs.s + "_" + newLhsNonTerminal.count(nt => nt.s.startsWith("alt" + lhs.s)));
        newLhsNonTerminal += altNT;

        ll.foreach(l => {
          val internalSymbols = new ListBuffer[Symbol]
          l.foreach(s2 => internalSymbols += transformSymbol(lhs, s2, prods));
          prods += Production(altNT, internalSymbols.toList)
        })

        return altNT;
      }
      case t: T => t
      case nt: NT => nt
    }
  }

  def concat(l: Traversable[Any]): String = {
    var s = "";
    l.foreach(o => o match {
      case l2: Traversable[Any] => s += concat(l2)
      case Opt(l2) => s += concat(l2);
      case Rep(l2) => s += concat(l2);
      case Alt(ll) => ll.foreach(l2 => s += concat(l2))
      case nt: NT => s += nt.s
      case t: T => s += t.s
    });
    return s;
  }
}