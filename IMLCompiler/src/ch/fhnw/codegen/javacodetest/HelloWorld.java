package ch.fhnw.codegen.javacodetest;

public class HelloWorld {

	private static Object tmp;
	private static int x;
	private static Object[] list;
	private static Object[] list2;
	private static boolean[] l2;

	public static void main(String[] args) {
		list = new Object[] { 100, 200 };

		tmp = list;
		list = new Object[list.length + 1];
		System.arraycopy(tmp, 0, list, 1, ((Object[]) tmp).length);
		list[0] = 0;

		// list2 = new Object[][][] { { { 100, 200, 300 } }, {} };
		// System.out.println((Object[]) list2[0]);

		// l2 = new boolean[] { true, false };
		// boolean b = l2[0];
		// System.out.println(b);

		// list = new int[] { 25030030, 1000, 1000, 1000, 1000, 1000, 1000 };
		// System.out.println(Arrays.toString(list));
		// System.out.println(Arrays.deepToString(list2));
		// list2 = new int[][] { { 100, 200, 300 }, { 400, 500, 600 } };

	}
}
