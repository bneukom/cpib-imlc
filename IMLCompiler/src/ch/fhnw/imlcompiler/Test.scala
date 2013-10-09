package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST.Program
import ch.fhnw.imlcompiler.AST._

object Test extends IMLParser {
  def main(args: Array[String]) {
    //    val p = "program a while x + 1 > (y*3)+42 do endwhile endprogram";
    //    val e = "true == true + 3 * 7";
    //    val i = "in const oink, out const boink";
    //    println(p.apply(s))

    val c = "if 3 * 3 > 4 do x:=x+1; x:= x-1 else skip endif";
    
    parseAll(cmd, c) match {
      case Success(result: Cmd, _) => println(result)
      case failure: NoSuccess => println("Parse Error: " + failure.toString)
    }

    //     def apply(input: String): Program = parseAll(program, input) match {
    //    case Success(result, _) => result
    //    case failure: NoSuccess => scala.sys.error(failure.msg)
    //  }
  }
}