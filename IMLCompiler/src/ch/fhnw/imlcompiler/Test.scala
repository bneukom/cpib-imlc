package ch.fhnw.imlcompiler

object Test extends IMLParser {
  def main(args: Array[String]) {
//    val p = "program hoink while x + 1 > (y*3)+42 do endwhile endprogram";
//    val e = "true == true + 3 * 7";
    val i = "in const oink, out const boink";
    //    println(p.apply(s))

    println(parseAll(globImpList, i));

  }
}