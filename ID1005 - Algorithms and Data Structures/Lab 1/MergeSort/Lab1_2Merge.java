/*Värsta fall av denna rutin kommer ge 2(n-2) + 1 jämförelser
 *dvs. då man i första steget i merge-rutinen tar element av arrayerna tills någon har 0 och de andra två har 1.
 *Sedan görs en jämförelse innan endast en array har element kvar, och det sista läggs till utan jämförelse.
 *Alltså är worst-case "långsammare" för 3-way än för vanlig mergesort.
 */

import java.util.*;

public class Lab1_2Merge {
	public static void main(String[] arg) {
		int[] list = new int[10];
		Random rand = new Random();
		for (int i = 0; i < list.length; i++) {
			list[i] = Math.abs(rand.nextInt()) % 100;
		}
		System.out.println(Arrays.toString(list));
		System.out.println(Arrays.toString(sort(list)));
	}
	//För att slippa skicka med arrayen i varje anrop skapas ett objekt där listan ingår som klassvariabel.
	public static int[] sort(int[] list) {
		return new mergeSort(list).sort(0, (list.length-1));
	}
}

class mergeSort {
	int[] list;
	public mergeSort(int[] list) {
		this.list = list;
	}
	//Dela upp arrayen i tre intervall och använd mergerutinen för att slå ihop delarna igen.
	public int[] sort(int firstIndex, int lastIndex) {
		if (firstIndex < lastIndex) {
			int third = (lastIndex - firstIndex) / 3;
			sort(firstIndex, firstIndex+third);
			sort(firstIndex + third + 1, firstIndex + 2 * third);
			sort(firstIndex + 2 * third + 1, lastIndex);
			merge(firstIndex, firstIndex+third, firstIndex + third + 1, firstIndex + 2 * third, firstIndex + 2 * third + 1, lastIndex);
		}
		return list;
	}

	public void merge(int leftFirst, int leftLast, int midFirst, int midLast, int rightFirst, int rightLast) {
		int[] result = new int[list.length];
		int index = leftFirst;
		int saveFirst = leftFirst;

		//Merge 3 sub-arrays tills någon är tom
		while((leftFirst <= leftLast) && (rightFirst <= rightLast) && (midFirst <= midLast)) {
			if (list[leftFirst] <= list[rightFirst] && list[leftFirst] <= list[midFirst]) {
				result[index] = list[leftFirst];
				leftFirst++;
			}
			else if (list[rightFirst] <= list[leftFirst] && list[rightFirst] <= list[midFirst]) {
				result[index] = list[rightFirst];
				rightFirst++;
			}
			else {
				result[index] = list[midFirst];
				midFirst++;
			}
			index++;
		}

		//Merge 2 sub-arrays, tills någon är tom. Här finns det tre olika "cases" beroende på vilka arrayer som är tomma
		//Endast ett av villkoren i de tre while-satserna går i uppfyllelse och körs dock.
		while((leftFirst <= leftLast) && (rightFirst <= rightLast)) {
			if (list[leftFirst] <= list[rightFirst]) {
				result[index] = list[leftFirst];
				leftFirst++;
			}
			else {
				result[index] = list[rightFirst];
				rightFirst++;
			}
			index++;
		}
		while((leftFirst <= leftLast) && (midFirst <= midLast)) {
			if (list[leftFirst] <= list[midFirst]) {
				result[index] = list[leftFirst];
				leftFirst++;
			}
			else {
				result[index] = list[midFirst];
				midFirst++;
			}
			index++;
		}
		while((midFirst <= midLast) && (rightFirst <= rightLast)) {
			if (list[midFirst] <= list[rightFirst]) {
				result[index] = list[midFirst];
				midFirst++;
			}
			else {
				result[index] = list[rightFirst];
				rightFirst++;
			}
			index++;
		}

		//Lägg till det sista av den sub-arrayen som har element kvar.
		//Endast ett av villkoren i de tre while-satserna går i uppfyllelse och körs dock.
		while (leftFirst <= leftLast) {
			result[index] = list[leftFirst];
			leftFirst++;
			index++;
		}
		while (rightFirst <= rightLast) {
			result[index] = list[rightFirst];
			rightFirst++;
			index++;
		}
		while (midFirst <= midLast) {
			result[index] = list[midFirst];
			midFirst++;
			index++;
		}
		//Flytta innehållet i den temporära resultat-arrayen till den array som ska returneras
		for (int i = saveFirst; i <= rightLast; i++) {
			list[i] = result[i];
		}
	}
}