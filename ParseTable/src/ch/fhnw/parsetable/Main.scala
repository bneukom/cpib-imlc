package ch.fhnw.parsetable

import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.mutable.ListBuffer

class ParseTableGenerator extends EBNFParsers with BNFTransformer with LL1 {

  def generateParseTable(ebnfGrammarString: String): ParseTable = {
    println("Input:")
    println(ebnfGrammarString)
    println()

    val ebnfGrammar = parse(ebnfGrammarString)
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
    println("Parse table generated")

    return parseTable;
  }

  def generateParseTableFromPath(path: String): ParseTable = {
    //    val file = scala.io.Source.fromFile("grammars/leftrecursionfactored.ebnf")
    val file = scala.io.Source.fromFile(path)
    //    val file = scala.io.Source.fromFile("grammars/parsetablegrammar.ebnf")
    //    val file = scala.io.Source.fromFile("grammars/leftrecursion.ebnf")
    val grammarString = file.mkString
    file.close()

    generateParseTable(grammarString)
  }

}

object Main {
  def main(args: Array[String]) {
    try {
      val p = new ParseTableGenerator;
      p.generateParseTableFromPath("grammars/imlgrammar.ebnf")
    } catch {
      case e: PraseTableException => e.printStackTrace();
    }
  }
}