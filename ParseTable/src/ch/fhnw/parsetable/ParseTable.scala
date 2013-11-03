package ch.fhnw.parsetable

import ch.fhnw.parsetable.BNFGrammar._

object ParseTable extends EBNFParsers with BNFTransformer with LL1 {
  def main(args: Array[String]) {

    val file = scala.io.Source.fromFile("grammars/factor.ebnf")
    val grammarString = file.mkString
    file.close()

    try {

      println("Input:")
      println(grammarString)
      println()

      val ebnfGrammar = parse(grammarString);
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
      nonterminals.foreach(nt => println(nt.s + " nullable: " + nullable(nt, transformedGrammar.prods)))
      nonterminals.foreach(nt => println(nt.s + " first: " + first(nt, transformedGrammar.prods)))
      nonterminals.foreach(nt => println(nt.s + " follow: " + follow(nt, transformedGrammar.prods, "e")))
    } catch {
      case e: PraseTableException => e.printStackTrace();
    }
  }

}