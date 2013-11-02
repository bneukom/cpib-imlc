package ch.fhnw.parsetable

object ParseTable extends EBNFParsers with BNFTransformer {
  def main(args: Array[String]) {

    val file = scala.io.Source.fromFile("grammars/factor.ebnf")
    val grammarString = file.mkString
    file.close()

    try {

      val ebnfGrammar = parse(grammarString);
      println("Parsed Grammar:")
      println(ebnfGrammar.treeString)

      val bnfGrammar = transform(ebnfGrammar);
      println("Converted EBNF to BNF")
      println(bnfGrammar.treeString)
    } catch  {
      case e:PraseTableException => e.printStackTrace();
    }
  }
}