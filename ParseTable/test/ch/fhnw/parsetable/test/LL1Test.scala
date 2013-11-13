package ch.fhnw.parsetable.test
import ch.fhnw.parsetable.BNFGrammar._
import scala.collection.mutable.ListBuffer
import ch.fhnw.parsetable.LL1

object LL1Test extends LL1 {
  def main(args: Array[String]) {
    testFollow
  }

  def testFollow() {
    val aNT = NT("a");
    val bNT = NT("b");
    val cNT = NT("c");
    val epsT = T("");
    val aT = T("A");
    val bT = T("B");
    val cT = T("C");

    // 
    //    val prods =
    //      Production(bNT, aT :: bNT :: aT :: Nil) ::
    //        Production(cNT, bNT :: bT :: Nil) :: Nil

    // c ::= Ab
    // b ::=  
    // b ::= a
    // a ::= Bb

    val prods2 =
      Production(cNT, aT :: bNT :: Nil) ::
        Production(bNT, epsT :: Nil) ::
        Production(bNT, aNT :: Nil) ::
        Production(aNT, bT :: bNT :: Nil) :: Nil
    //      Production(cNT, bNT :: Nil) ::
    //        Production(bNT, cNT :: Nil) :: Nil

    val grammar = Grammar(prods2);

    println("grammar:");
    printGrammar(grammar);
    println();

    val nts = prods2.collect({ case x => x.l }).distinct

    println("follow:");
    nts.foreach(nt => println(nt.s + " nul: " + nullable(nt, prods2, ListBuffer[NT]())))
    nts.foreach(nt => println(nt.s + " first: " + first(nt, prods2)))
    nts.foreach(nt => println(nt.s + " follow: " + follow(nt, prods2, cNT.s)))
  }

  def testFirst() {
    val aNT = NT("a");
    val bNT = NT("b");
    val cNT = NT("c");
    val epsT = T("");
    val aT = T("A");
    val bT = T("B");
    val cT = T("C");
    val prods =
      Production(aNT, epsT :: Nil) ::
        Production(aNT, bT :: Nil) ::
        Production(aNT, aT :: Nil) ::
        Production(bNT, aNT :: aT :: Nil) ::
        Production(cNT, cT :: Nil) ::
        Production(cNT, bNT :: Nil) :: Nil

    val grammar = Grammar(prods);

    println("grammar:");
    printGrammar(grammar);
    println();

    val nts = prods.collect({ case x => x.l }).distinct

    println("first:");
    nts.foreach(nt => println(nt.s + " first: " + first(nt, prods)))
  }

  def testNullable() {
    val aNT = NT("a");
    val bNT = NT("b");
    val cNT = NT("c");
    val epsT = T("");
    val aT = T("A");
    val prods =
      Production(aNT, epsT :: Nil) ::
        Production(aNT, aT :: Nil) ::
        Production(bNT, aT :: Nil) ::
        Production(bNT, aNT :: aT :: Nil) ::
        Production(cNT, aT :: Nil) ::
        Production(cNT, aNT :: Nil) :: Nil

    val grammar = Grammar(prods);

    println("grammar:");
    printGrammar(grammar);
    println();

    val nts = prods.collect({ case x => x.l }).distinct

    println("nullables:");
    nts.foreach(nt => println(nt.s + " nullable: " + nullable(nt, prods, ListBuffer[NT]())))
  }
}