/*Komplexitet för att
 *lägga till element: Balanserat träd: O(nlogn), Obalanserat träd (worst): O(n^2)
 *ta ut element: Balanserat träd: O(nlogn), Obalanserat träd (worst): O(n^2)
 *
 *
 *
 *
 */

import java.util.*;

public class treeSort {
	public static void main(String[] arg) {
		int[] list = new int[10];
		Random rand = new Random();
		//Skapar en array med random tal.
		for (int i = 0; i < list.length; i++) {
			list[i] = Math.abs(rand.nextInt()) % 100;
		}
		//Visar listan före och efter sortering.
		System.out.println(Arrays.toString(list));
		System.out.println(Arrays.toString(sort(list, 0)));
		System.out.println(Arrays.toString(sort(list, 1)));
	}

	public static int[] sort(int[] list, int dir) {
		return new treeSorter(list).sort(dir);
	}
}


class treeSorter {
	int[] list;
	public treeSorter(int[] list) {
		this.list = list;
	}

	public int[] sort(int dir) {
		lhQueue<Integer> q = new lhQueue<Integer>();
		//Lägg elementen från arrayen till kön/trädet.
		for (int i = 0; i < list.length; i++) {
			q.put(list[i]);
		}
		System.out.println(q.size());
		//Ta ut det lägsta elementet och placera det i listan.
		if (dir == 0) {
			for (int i = 0; i < list.length; i++) {
				list[i] = q.takeLowest();
			}
		}
		//Ta ut det högsta elementet och placera det i listan.
		else {
			for (int i = 0; i < list.length; i++) {
				list[i] = q.takeHighest();
			}
		}
		System.out.println(q.size());
		return list;
	}
}