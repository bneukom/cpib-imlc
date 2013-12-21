package ch.fhnw.codegen.javacodetest;

public class ListConsTest {

	public static boolean foo(int i, int j) {
		return i > j;
	}
	
	public static void main(String[] args) {
		boolean b = foo(3, 3);
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
	// a ++;
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
