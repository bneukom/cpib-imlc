package ch.fhnw.parsetable

import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.mutable.ListBuffer

object ParseTable extends EBNFParsers with BNFTransformer with LL1 {
  def main(args: Array[String]) {

    //    val file = scala.io.Source.fromFile("grammars/leftrecursionfactored.ebnf")
    val file = scala.io.Source.fromFile("grammars/imlwithlistsgrammar.ebnf")
    //    val file = scala.io.Source.fromFile("grammars/parsetablegrammar.ebnf")
    //    val file = scala.io.Source.fromFile("grammars/leftrecursion.ebnf")
    val grammarString = file.mkString
    file.close()

    try {

      println("Input:")
      println(grammarString)
      println()

      val ebnfGrammar = parse(grammarString)
      val startSymbol = ebnfGrammar.prods.head.l.s

      println("Parsed Grammar:")
      println(ebnfGrammar)
      println()

      val transformedGrammar = transform(ebnfGrammar);
      println("Converted EBNF to default grammar")
      println(transformedGrammar)
      println()
      println("Converted Grammar:")
      printGrammar(transformedGrammar);
      println()

      val nonterminals = nts(transformedGrammar.prods);
      println("NTS: " + nonterminals)
      println()

      nonterminals.foreach(nt => {
        println(nt.s + " nullable: " + nullable(nt, transformedGrammar.prods, ListBuffer[NT]()))
        println(nt.s + " first: " + first(nt, transformedGrammar.prods))
        println(nt.s + " follow: " + follow(nt, transformedGrammar.prods, startSymbol))
      })

      println()
      val parseTable = genParseTable(transformedGrammar.prods, startSymbol)
      println(parseTable)

    } catch {
      case e: PraseTableException => e.printStackTrace();
    }
  }

}