package ch.fhnw.parsetable

import scala.collection.mutable.ListBuffer

import ch.fhnw.parsetable.BNFGrammar.NT
import ch.fhnw.parsetable.BNFGrammar.PraseTableException
import ch.fhnw.parsetable.BNFGrammar.checkGrammar
import ch.fhnw.parsetable.BNFGrammar.printGrammar

class ParseTableGenerator extends EBNFParsers with BNFTransformer with LL1 {

  def generateParseTable(ebnfGrammarString: String): ParseTable = {
    println("Input:")
    println(ebnfGrammarString)
    println()

    val ebnfGrammar = parse(ebnfGrammarString)

    if (ebnfGrammar.prods.isEmpty) return new ParseTable(List(), List(), Array());

    val startProduction = ebnfGrammar.prods.head
    val startSymbol = startProduction.l.s

    println("Check Grammar:")
    System.out.flush();

    val res = checkGrammar(ebnfGrammar, startProduction);
    res.foreach(x => {
      if (!x._2._1) System.err.println("unreachable nonterminal found: " + x._1.s)
      if (!x._2._2 && x._1.s != startSymbol) System.err.println("Warning; undefined nonterminal used: " + x._1.s)
    })
    System.err.flush();

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
    println()
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
    //    try {
    //      val p = new ParseTableGenerator;
    //      p.generateParseTableFromPath("grammars/imlwithlistsgrammar.ebnf")
    //    } catch {
    //      case e: PraseTableException => System.err.println(e.getMessage())
    //    }

    val l = List();
  }

}