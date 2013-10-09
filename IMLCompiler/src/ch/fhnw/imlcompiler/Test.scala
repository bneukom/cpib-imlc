package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST._

object Test extends IMLParser {
  def main(args: Array[String]) {
    //    val p = "program a while x + 1 > (y*3)+42 do endwhile endprogram";
    //    val e = "true == true + 3 * 7";
    //    val i = "in const oink, out const boink";
    //    println(p.apply(s))

    //    val c = "program intDiv global proc divide(in copy m:int, in copy n:int, out ref q:int, out ref r:int) do q init:= 0; r init:= m; whiler >= n do q := q + 1; r := r -n endwhile endproc; var m:int; var n:int; var q:int; var r:int do ? m init; ? n init; call divide(m, n, q init, r init); ! q; ! r endprogram";
    val cpsDeclTest = "const l:int fun oink(out ref var i:int) returns l:int do i:=(i*3) mod 2 div 3 endfun";
    //    val cpsDeclTest = "const l:int proc";

    parseAll(cpsDecl, cpsDeclTest) match {
      case Success(result: CpsDecl, _) => println("Parse Result: " + result)
      case failure: NoSuccess => println("Parse Error: " + failure.toString)
    }

    //     def apply(input: String): Program = parseAll(program, input) match {
    //    case Success(result, _) => result
    //    case failure: NoSuccess => scala.sys.error(failure.msg)
    //  }
  }
}