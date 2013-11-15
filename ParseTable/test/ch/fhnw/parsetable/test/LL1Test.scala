package ch.fhnw.parsetable.test

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import ch.fhnw.parsetable.BNFGrammar._
import ch.fhnw.parsetable.LL1
import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class LL1TestC extends FunSuite with BeforeAndAfter with LL1 {

  val aNT = NT("a");
  val bNT = NT("b");
  val cNT = NT("c");
  val epsT = T("");
  val aT = T("A");
  val bT = T("B");
  val cT = T("C");

  val nts = List(aNT, bNT, cNT)

  test("follow") {

    val prods =
      Production(cNT, aT :: bNT :: Nil) ::
        Production(bNT, epsT :: Nil) ::
        Production(bNT, aNT :: Nil) ::
        Production(aNT, bT :: bNT :: Nil) :: Nil

    val grammar = Grammar(prods);

    assert(follow(aNT, prods, cNT.s) == Set(T("$")))
    assert(follow(bNT, prods, cNT.s) == Set(T("$")))
    assert(follow(cNT, prods, cNT.s) == Set(T("$")))
  }

  test("follow2") {

  }

  test("first") {
    val prods =
      Production(aNT, epsT :: Nil) ::
        Production(aNT, bT :: Nil) ::
        Production(aNT, aT :: Nil) ::
        Production(bNT, aNT :: aT :: Nil) ::
        Production(cNT, cT :: Nil) ::
        Production(cNT, bNT :: Nil) :: Nil

    assert(first(aNT, prods) == Set(bT, aT))
    assert(first(bNT, prods) == Set(bT, aT))
    assert(first(cNT, prods) == Set(bT, cT, aT))
  }

  test("nullable") {
    val prods =
      Production(aNT, epsT :: Nil) ::
        Production(aNT, aT :: Nil) ::
        Production(bNT, aT :: Nil) ::
        Production(bNT, aNT :: aT :: Nil) ::
        Production(cNT, aT :: Nil) ::
        Production(cNT, aNT :: aNT :: Nil) :: Nil

    assert(nullable(aNT, prods, ListBuffer[NT]()) == true)
    assert(nullable(bNT, prods, ListBuffer[NT]()) == false)
    assert(nullable(cNT, prods, ListBuffer[NT]()) == true)
  }
}