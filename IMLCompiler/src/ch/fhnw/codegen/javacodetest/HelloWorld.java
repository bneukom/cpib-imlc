package ch.fhnw.codegen.javacodetest;

public class HelloWorld {

	private static int x;
	private static int[] list;
	private static Object list2;

	public static void main(String[] args) {
		// list = new int[] { 500, 200, 3000, 2000, 10000, 10000, 30000, 50000
		// };
		// list2 = new int[][] { { 10, 11, 12 }, { 11, 12, 13 } };
		// list2 = new int[][] { { 1000 } };
		// int a = 10;
		// list2 = new int[] { 10 * a, 1000, 1000, 1000, 1000, 1000, 1000 };
		list2 = new int[][] { { 100, 200, 300 }, { 400, 500, 600 } };
	}
}
