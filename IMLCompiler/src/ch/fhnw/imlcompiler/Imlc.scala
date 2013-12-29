package ch.fhnw.imlcompiler

object Imlc {
  def main(args: Array[String]) {
    if (args.length == 1) {
      val path = args(0);

      ImlCompiler.compile(path, false);
    } else if (args.length == 2) {
      val flag = args(0);

      if (flag == "-d") {
    	  ImlCompiler.compile(args(1), true);
      } else {
        System.err.println("invalid flag: " + flag)
        System.err.println
         printUsage;
      }
    } else {
      System.err.println("invalid amount of parameters");
      System.err.println
       printUsage;
    }
  }

  def printUsage() {
    println("Usage: imlc [-d] imlprogram.iml");
    println("-d: Print debug information");
  }

}