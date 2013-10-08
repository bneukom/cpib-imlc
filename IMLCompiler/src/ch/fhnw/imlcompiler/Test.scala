package ch.fhnw.imlcompiler

object Test {
  def main(args: Array[String]) {
    val s = "program while 341*43 do endwhile endprogram";
    val p = new IMLParser()
    //    println(p.apply(s))
//    println(p.apply("+3 * +3"))
//    println(p.apply("program while while x+1 mod 3 + 3 != 3+3 do endwhile endprogram"))
    println(p.apply("1 + 3 == 5"))

    val x = 12;
    val y = 1 + 3 * 4 + 1 == 5 + 1 * 3;
    println(y)
  }
}