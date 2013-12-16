package ch.fhnw.imlcompiler.transforming;

public class ResultTest {


	// TODO what if nested :: operations are used (does it still work)?
	// used for :: operations
	static Object[] tmp;

	public static void addThree(int[] value /* in ref int32 */) { // proc
		value[0] = (int) value[0] + 3;
	}

	// if by value direct access, else via Wrapper
	public static void addThree2(int value /* in copy int32 */) { // proc
		value = value + 3;
	}

	/*
	 * list init := [1,2,3,4];
	 * list := 0 :: list;
	 * debugout list;
	 * 
	 * nestedList init := [[1,2],[3,4]];
	 * nestedList := [-2,-1,0] :: nestedList;
	 * debugout nestedList;
	 * 
	 * intValue init := head list;
	 * debugout intValue;
	 * 
	 * list := head nestedList;
	 * debugout list;
	 * 
	 * list := tail list;
	 */
	public static void main(String[] args) {
		// globals
		int intValue = 0; // intValue:int;
		Object[] list; // list:[int];
		Object[] nestedList; // list:[int];
		
		// list init := [1,2,3,4];
		list = new Object[] { 1, 2, 3, 4 };

		// list := 0 :: list;
		tmp = (Object[]) list;
		list = new Object[((Object[]) list).length + 1];
		System.arraycopy(tmp, 0, list, 1, tmp.length);
		((Object[]) list)[0] = 0;

		// debugout list;
		printarr((Object[]) list);

		// nestedList init := [[1,2],[3,4]];
		nestedList = new Object[] { new Object[] { 1, 2 }, new Object[] { 3, 4 } };

		// nestedList := [-2,-1,0] :: nestedList;
		tmp = (Object[]) nestedList;
		nestedList = new Object[((Object[]) nestedList).length + 1];
		System.arraycopy(tmp, 0, nestedList, 1, tmp.length);
		((Object[]) nestedList)[0] = new Object[] { -2, -1, 0 };

		// debugout nestedList;
		printarr((Object[]) nestedList);

		// intValue init := head list;
		intValue = (int) list[0];
		System.out.println(intValue);

		// list := head nestedList;
		list = (Object[]) nestedList[0]; // possible due to immutability of lists

		// debugout list;
		printarr(list);

		list = new Object[((Object[]) nestedList).length - 1];
		System.arraycopy(nestedList, 1, list, 0, nestedList.length - 1);

		// debugout list;
		printarr(list);
	}

	private static void printarr(Object[] arr) {
		printarr(arr, true);
	}

	private static void printarr(Object[] arr, boolean newline) {
		System.out.print("[");
		for (int i = 0; i < arr.length; ++i) {
			Object val = arr[i];
			if (val instanceof Object[]) {
				printarr((Object[]) val, false);
			} else {
				System.out.print(val);
			}

			if (i < arr.length - 1)
				System.out.print(", ");
		}
		System.out.print("]" + (newline ? "\n" : ""));
	}

}
	