package ch.fhnw.codegen.javacodetest;


public class ListConsTest {

	private static int[] i = new int[1];
	private static int[] i2 = new int[1];
	private static int[] i3 = new int[1];
	
	private static void foo(int[] x /*inout*/, int[] x2 /*ref*/, int[] x3 /*out*/) {
		
	}
	
	public static void main(String[] args) {
		// local var a
		int[] tmp0 = new int[1];
		tmp0[0] = i[0];
		int[] tmp1 = new int[1];
		foo(tmp0, i2, tmp1);
		i[0] = tmp0[0];
		i3[0] = tmp1[0];
	}

	// // globals
	// private static int intValue = 0; // intValue:int;
	// private static Object[] list; // list:[int];
	// private static Object[] nestedList; // list:[int];
	//
	// public static void main(String[] args) {
	//
	// int a = 3;
	// while (a > 5) {
	// a++;
	// }
	//
	// // list init := [1,2,3,4];
	// list = new Object[] { 1, 2, 3, 4 };
	//
	// // list := -1 :: 0 :: list;
	// list = cons(list);
	// list[0] = 0;
	//
	// list = cons(list);
	// list[0] = -1;
	//
	// // debugout list;
	// System.out.println(Arrays.toString(list));
	//
	// // nestedList = [[1,2,3],[3,4,5]];
	// nestedList = new Object[] { new Object[] { 1, 2, 3 }, new Object[] { 3, 4, 5 } };
	//
	// // debugout nestedList;
	// System.out.println(Arrays.deepToString(nestedList));
	//
	// // list := head nestedList
	// list = (Object[]) deepCopy(nestedList[0]);
	//
	// // debugout list;
	// System.out.println(Arrays.toString(list));
	//
	// // nestedList := (40 :: [50,60,70]) :: nestedList;
	// nestedList = cons(nestedList);
	// Object[] tmp = cons(new Object[] { 50, 60, 70 });
	// tmp[0] = 40;
	// nestedList[0] = tmp;
	//
	// // debugout nestedList;
	// System.out.println(Arrays.deepToString(nestedList));
	//
	// // nestedList := tail nestedList;
	// nestedList = tail(nestedList);
	//
	// // debugout nestedList;
	// System.out.println(Arrays.deepToString(nestedList));
	// }
	//
	// private static Object[] tail(Object[] o) {
	//
	// final Object[] result = new Object[o.length - 1];
	//
	// for (int i = 1; i < o.length; ++i) {
	// Object object = o[i];
	// if (object instanceof Object[]) {
	// result[i - 1] = deepCopy(object);
	// } else {
	// result[i - 1] = object;
	// }
	// }
	//
	// return result;
	// }
	//
	// private static Object[] cons(Object[] o) {
	//
	// final Object[] result = new Object[o.length + 1];
	//
	// for (int i = 0; i < o.length; ++i) {
	// Object object = o[i];
	// if (object instanceof Object[]) {
	// result[i + 1] = deepCopy(object);
	// } else {
	// result[i + 1] = object;
	// }
	// }
	//
	// return result;
	// }
	//
	// private static Object deepCopy(Object o) {
	// if (o instanceof Object[]) {
	// final Object[] l = (Object[]) o;
	// final Object[] result = new Object[l.length];
	// for (int i = 0; i < l.length; ++i) {
	// Object object = l[i];
	// if (object instanceof Object[]) {
	// result[i] = deepCopy(object);
	// } else {
	// result[i] = object;
	// }
	// }
	// return result;
	// } else {
	// return o;
	// }
	// }
}
