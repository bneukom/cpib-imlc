package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST._
import scala.text._
import Document._

object Compiler extends IMLParsers with ContextCheckers {

  def main(args: Array[String]) {
    //    val p = "program a while x + 1 > (y*3)+42 do endwhile endprogram";
    //    val e = "true == true + 3 * 7";
    //    val i = "in const oink, out const boink";
    //    println(p.apply(s))
    //    val intDivProg = "program intDiv global proc divide(in copy m:int, in copy n:int, out ref q:int, out ref r:int) do q init:= 0; r init:= m; while r >= n do q := q + 1; r := r - n endwhile endproc; var m:int; var n:int; var q:int; var r:int do input m init; input n init; call divide(m, n, q init, r init); output q; output r endprogram";
    //    val cpsDeclTest = "const l:int fun oink(out ref var i:int) returns l:int do i:=(i*3) mod 2 div 3 endfun";
    //	  val expression = "3 * 3 + 4 * (6 > 7)";

    try {
      //      val prog = "program test global proc test(in copy m:bool, out copy m:int, y:int, z:int) do m:=3 * x + 4 > y && z > 10 endproc; var x:int; proc test2(in copy m:int, out copy a:int) do m:=m div (m * (a + 1)) endproc do skip endprogram"
      val prog = "program test global proc test(in copy m:int, in copy a:bool, x:int, z:int) do call test(3 + 3 * 3,true,3, 3) endproc do skip endprogram"

      // parse
      val parseResult = parse(prog)
      println("Parse Successful:")
      println(parseResult.treeString)
      println()

      // context check
      check(parseResult);

      // interpret
    } catch {
      case e: ParseException => System.err.println(e.getMessage)
      case e: CompilerException => System.err.println(e.getMessage)
    }

  }

}