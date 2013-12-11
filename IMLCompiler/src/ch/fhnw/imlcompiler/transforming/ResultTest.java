package ch.fhnw.imlcompiler.transforming;

public class ResultTest {

	// TODO globals should not be here! (They're only visible in main method)
	// global
	static Ref _intValue = new Ref(); // intValue:int;
	static Ref _list = new Ref(); // list:[int];
	static Ref _nestedList = new Ref(); // list:[int];

	// TODO what if nested :: operations are used (does it still work)?
	// used for :: operations
	static Object[] tmp;

	public static void addThree(Ref _value /* in ref int32 */) { // proc
		_value.value = (int) _value.value + 3;
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
		// list init := [1,2,3,4];
		_list.value = new Object[] { 1, 2, 3, 4 };

		// list := 0 :: list;
		tmp = (Object[]) _list.value;
		_list.value = new Object[((Object[]) _list.value).length + 1];
		System.arraycopy(tmp, 0, _list.value, 1, tmp.length);
		((Object[]) _list.value)[0] = 0;

		// debugout list;
		printarr((Object[]) _list.value);

		// nestedList init := [[1,2],[3,4]];
		_nestedList.value = new Object[] { new Object[] { 1, 2 }, new Object[] { 3, 4 } };

		// nestedList := [-2,-1,0] :: nestedList;
		tmp = (Object[]) _nestedList.value;
		_nestedList.value = new Object[((Object[]) _nestedList.value).length + 1];
		System.arraycopy(tmp, 0, _nestedList.value, 1, tmp.length);
		((Object[]) _nestedList.value)[0] = new Object[] { -2, -1, 0 };

		// debugout nestedList;
		printarr((Object[]) _nestedList.value);

		// intValue init := head list;
		_intValue.value = ((Object[]) _list.value)[0];
		System.out.println(_intValue.value);

		// list := head nestedList;
		_list.value = ((Object[]) _nestedList.value)[0]; // possible due to immutability of lists

		// debugout list;
		printarr((Object[]) _list.value);

		_list.value = new Object[((Object[]) _nestedList.value).length - 1];
		System.arraycopy(_nestedList.value, 1, _list.value, 0, ((Object[]) _nestedList.value).length - 1);

		// debugout list;
		printarr((Object[]) _list.value);
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

	// TODO or just use an array?
	// by reference wrapper
	private static class Ref {
		public Object value;
	}
}
