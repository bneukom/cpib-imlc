package ch.fhnw.codegen.javacodetest;

public class HelloWorld {

	private static int x;
	private static int[] list;
	private static int[][] list2;
	private static boolean[] l2;

	public static void main(String[] args) {
		// list = new int[] { 500, 200, 3000 };
		// int a = list[0];
		// System.out.println(x);
		//
		// list2 = new int[][] { { 500, 600 } };
		// int[] b = list2[0];
		// System.out.println(x);

		l2 = new boolean[] { true, false };
		boolean b = l2[0];
		System.out.println(b);

		// list = new int[] { 25030030, 1000, 1000, 1000, 1000, 1000, 1000 };
		// System.out.println(Arrays.toString(list));
		// System.out.println(Arrays.deepToString(list2));
		// list2 = new int[][] { { 100, 200, 300 }, { 400, 500, 600 } };

	}
}
