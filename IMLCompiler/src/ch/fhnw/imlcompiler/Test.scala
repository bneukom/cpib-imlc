package ch.fhnw.imlcompiler

object Test {
  def main(args: Array[String]) {
    val s = "program while 341*43 do endwhile endprogram";
    val p = new IMLParser()
    //    println(p.apply(s))
    println(p.apply("3 + 1 == 4"))
  }
}