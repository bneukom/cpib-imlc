package ch.fhnw.imlcompiler

object Test extends IMLParser {
  def main(args: Array[String]) {
    val s = "program hoink while x + 1 > (y*3)+42 do endwhile endprogram";
    //    println(p.apply(s))

    println(parseAll(program, s));

  }
}