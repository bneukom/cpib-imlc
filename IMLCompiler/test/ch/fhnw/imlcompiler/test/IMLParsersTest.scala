package ch.fhnw.imlcompiler.test

import ch.fhnw.imlcompiler.AST._
import ch.fhnw.imlcompiler.parsing.IMLParsers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfter
import org.scalatest.junit.JUnitRunner
import ch.fhnw.imlcompiler.AST._
import org.scalatest.Matchers
import ch.fhnw.imlcompiler.checking.CompilerException

@RunWith(classOf[JUnitRunner])
class IMLParsersTest extends FunSuite with BeforeAndAfter with IMLParsers with Matchers {

  test("expr: operator precedence") {
    val intDivProg = "3 * -x + -4 > y && z > 10";
    val parseResult = parse(intDivProg, expr);

    assert(parseResult == DyadicExpr(DyadicExpr(DyadicExpr(DyadicExpr(LiteralExpr(IntLiteral(3)), TimesOpr, MonadicExpr(StoreExpr(Ident("x"), false), MinusOpr)), PlusOpr, MonadicExpr(LiteralExpr(IntLiteral(4)), MinusOpr)), GT, StoreExpr(Ident("y"), false)), Cand, DyadicExpr(StoreExpr(Ident("z"), false), GT, LiteralExpr(IntLiteral(10)))))
  }

  test("expr: brackets") {
    val brackets = "((((x + 1))))";
    val parseResult = parse(brackets, expr);

    assert(parseResult == DyadicExpr(StoreExpr(Ident("x"), false), PlusOpr, LiteralExpr(IntLiteral(1))))
  }
  
  test("expr: lists") {
    val cons = "1 :: 2 :: []";
    parse(cons, expr) shouldEqual DyadicExpr(LiteralExpr(IntLiteral(1)),ConsOpr, DyadicExpr(LiteralExpr(IntLiteral(2)), ConsOpr, LiteralExpr(ListLiteral(List()))))
  }

  test("literals") {
    val intLit = "1";
    val boolLit = "true";
    val listLit = "[1,2,3]";

    assert(parse(intLit, literal) == IntLiteral(1))
    assert(parse(boolLit, literal) == BoolLiteral(true))
    assert(parse(listLit, literal) == ListLiteral(List(LiteralExpr(IntLiteral(1)), LiteralExpr(IntLiteral(2)), LiteralExpr(IntLiteral(3)))))
  }

  test("identifier") {
    parse("x", ident) shouldEqual Ident("x")
    parse("x1", ident) shouldEqual Ident("x1")
    
    a[CompilerException] should be thrownBy parse("_x", ident)
    a[CompilerException] should be thrownBy parse("while", ident)
  }
}