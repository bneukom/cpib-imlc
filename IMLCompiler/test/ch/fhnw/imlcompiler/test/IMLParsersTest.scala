package ch.fhnw.imlcompiler.test

import ch.fhnw.imlcompiler.AST._
import ch.fhnw.imlcompiler.IMLParsers

object IMLParsersTest extends IMLParsers {
  def main(args: Array[String]) {
    val intDivProg = "3 * -x + -4 > y cand z > 10a";
    //    val cpsDeclTest = "const l:int fun oink(out ref var i:int) returns l:int do i:=(i*3) mod 2 div 3 endfun";

    // parse
    val parseResult = parse(intDivProg);
    print(parseResult);
  }
}