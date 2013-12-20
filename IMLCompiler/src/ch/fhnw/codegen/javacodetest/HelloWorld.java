package ch.fhnw.codegen.javacodetest;

import java.util.Arrays;

public class HelloWorld {

	// private static Object tmp;
	// private static int x;
	// private static Object[] list;
	// private static Object[] list2;
	// private static boolean[] l2;
	//
	// public static void main(String[] args) {
	// list = new Object[] { 100, 200 };
	// tmp = list;
	// list = new Object[list.length + 1];
	// System.arraycopy(tmp, 0, list, 1, ((Object[]) tmp).length);
	// list[0] = 0;
	//
	// // list2 = new Object[][][] { { { 100, 200, 300 } }, {} };
	// // System.out.println((Object[]) list2[0]);
	//
	// // l2 = new boolean[] { true, false };
	// // boolean b = l2[0];
	// // System.out.println(b);
	//
	// // list = new int[] { 25030030, 1000, 1000, 1000, 1000, 1000, 1000 };
	// // System.out.println(Arrays.toString(list));
	// // System.out.println(Arrays.deepToString(list2));
	// // list2 = new int[][] { { 100, 200, 300 }, { 400, 500, 600 } };
	//
	// }

	private static Object[] l;
	private static Object[] l2;
	private static Object _tmp;
	private static Object _tmp2;
	private static Object _tmp3;
	private static Object _tmp4;

	public static void main(String[] paramArrayOfString) {
		l2 = new Object[][] { { Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3) }, { Integer.valueOf(4), Integer.valueOf(5), Integer.valueOf(6) } };
		_tmp = l2;
		_tmp2 = new Object[((Object[]) _tmp).length + 1][];
		System.arraycopy(_tmp, 0, _tmp2, 1, ((Object[]) _tmp).length);
		_tmp3 = new Object[] { Integer.valueOf(0), Integer.valueOf(100) };
		_tmp4 = new Object[((Object[]) _tmp).length + 1];
		System.arraycopy(_tmp3, 0, _tmp4, 1, ((Object[]) _tmp3).length);
		((Object[]) _tmp2)[0] = Integer.valueOf(1);
		((Object[]) _tmp3)[0] = ((Object[]) _tmp2);
		l2 = (Object[]) _tmp2;
		System.out.println(Arrays.deepToString(l2));
	}
}
