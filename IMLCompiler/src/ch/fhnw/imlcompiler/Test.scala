package ch.fhnw.imlcompiler

object Test {
  def main(args: Array[String]) {
    val s = "program while 341*43 do endwhile endprogram";
    val p = new IMLParser()
    //    println(p.apply(s))
//    println(p.apply("+3 * +3"))
    println(p.apply("program while while while mod 3 + 3 != 3+3 do endwhile endprogram"))

    val x = 12;
    val y = x % 1 + 2340;
    println(y)
  }
}