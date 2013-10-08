package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST.Program

object Test extends IMLParser {
  def main(args: Array[String]) {
    val p = "program a while x + 1 > (y*3)+42 do endwhile endprogram";
    //    val e = "true == true + 3 * 7";
    //    val i = "in const oink, out const boink";
    //    println(p.apply(s))

    parseAll(program, p) match {
      case Success(result: Program, _) => println(result)
      case failure: NoSuccess => println("Parse Error: " + failure.toString)
    }

//     def apply(input: String): Program = parseAll(program, input) match {
//    case Success(result, _) => result
//    case failure: NoSuccess => scala.sys.error(failure.msg)
//  }
  }
}