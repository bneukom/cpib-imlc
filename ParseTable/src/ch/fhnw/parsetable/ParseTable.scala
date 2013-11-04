package ch.fhnw.parsetable

import scala.collection.JavaConversions._
import ch.fhnw.parsetable.BNFGrammar._

class ParseTable(terminals: List[T], nonTerminals: List[NT], table: Array[Array[List[Symbol]]]) {
  
  // used for java access
  def terminals2(): java.util.List[T] = terminals;
  def nonTerminals2(): java.util.List[NT] = nonTerminals;
  def table2(): Array[Array[java.util.List[Symbol]]] = {
    val res = Array.ofDim[java.util.List[Symbol]](nonTerminals.size, terminals.size)
    for (x <- 0 until table.length) {
      res(x) = Array.ofDim[java.util.List[Symbol]](table(x).length)
      for (y <- 0 until table(x).length) {
        if (table(x)(y) != null && table(x)(y).size > 0)
          res(x)(y) = table(x)(y)
        else res(x)(y) = new java.util.ArrayList[Symbol]();
      }
    }
    res;
  }
}