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
    val oinkT = T("OINK");
    val boinkT = T("BOINK");
    val troinkT = T("TROINK");
    //    val prods =
    //      Production(aNT, epsT :: Nil) ::
    //        Production(aNT, boinkT :: Nil) ::
    //        Production(aNT, oinkT :: Nil) ::
    //        Production(bNT, aNT :: oinkT :: Nil) ::
    //        Production(cNT, troinkT :: Nil) ::
    //        Production(cNT, bNT :: Nil) :: Nil

    // 
    val prods =
      Production(bNT, oinkT :: bNT :: Nil) :: Nil
    // TODO inderect left recursion will fail
    //    val prods =
    //      Production(bNT, aNT :: Nil) ::
    //        Production(aNT, bNT :: Nil) :: Nil

    val grammar = Grammar(prods);

    println("grammar:");
    printGrammar(grammar);
    println();

    val nts = prods.collect({ case x => x.l }).distinct

    println("follow:");
    nts.foreach(nt => println(nt.s + " follow: " + follow(nt, prods)))
  }

  def testFirst() {
    val aNT = NT("a");
    val bNT = NT("b");
    val cNT = NT("c");
    val epsT = T("");
    val oinkT = T("OINK");
    val boinkT = T("BOINK");
    val troinkT = T("TROINK");
    val prods =
      Production(aNT, epsT :: Nil) ::
        Production(aNT, boinkT :: Nil) ::
        Production(aNT, oinkT :: Nil) ::
        Production(bNT, aNT :: oinkT :: Nil) ::
        Production(cNT, troinkT :: Nil) ::
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
    val oinkT = T("OINK");
    val prods = Production(aNT, epsT :: Nil) ::
      Production(aNT, oinkT :: Nil) ::
      Production(bNT, oinkT :: Nil) ::
      Production(bNT, aNT :: oinkT :: Nil) ::
      Production(cNT, oinkT :: Nil) ::
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