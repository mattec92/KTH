/*FL-kö
 *Komplexiteter:
 *takeLast: Oberoende av antalet element. Komplexiteten är därmed O(1) (O = theta)
 *takeFirst: Flyttar alla element ett steg mot början där hål uppstår när ett element tas ut. Komplexiteten O(n)
 *putLast: (put) Oberoende av antal element. Komplexiteten O(1)
 *putFirst: Flyttar alla element ett steg bakåt i sekvensen. Komplexiteten O(n)
 *isFull, isEmpty, size: Oberoende av antal element. Komplexiteten O(1)
 */

import java.util.*;

//Last in First out - kö där endast tal mindre än alla i kön kan läggas till.
public class flQueue<E> {
	private E[] elements;
	private int lastIndex = -1;
	final int DEFAULT_CAPACITY = 100;
	final int ENLARGEVALUE = 50;
	private Comparator<E> comparator;

	private class Comp implements Comparator<E> {
		public int compare (E element1, E element2) {
			Comparable<E> element = (Comparable<E>) element1;
			return element.compareTo(element2);
		}
	}

	//Skapar en ny kö med DEFAULT_CAPACITY (100) som storlek
	public flQueue() {
		elements = (E[]) (new Object[DEFAULT_CAPACITY]);
		comparator = new Comp();
	}
	//Returnerar hur många element som finns i kön
	public int size() {
		return lastIndex + 1;
	}
	//Kollar om kön är full
	public boolean isFull() { //Behövs inte i en non-bounded queue.
		return false;
	}
	//Kollar om kön är tom.
	public boolean isEmpty() {
		return lastIndex == -1;
	}
	//Lägger till ett element sist i kön.
	public String putLast(E element) {
		if (lastIndex == elements.length - 1) {
			this.enlarge();
		}
		elements[lastIndex+1] = element;
		lastIndex++;
		return element.toString();
	}
	//Lägger till ett element först i kön.
//	public void putFirst(E element) {
//		if (lastIndex == elements.length - 1) {
//			this.enlarge();
//		}
//		for (int i = lastIndex; i > 0; i--) {
//			elements[i+1] = elements[i];
//		}
//		elements[0] = element;
//		lastIndex++;
//	}
	//Kollar på nästa element i kön, i denna implementation: sista elementet.
	public E peek() {
		return elements[lastIndex];
	}
	//Tar ut det sita elementet ur kön.
	public E takeLast() {
		if (this.isEmpty()) {
			throw new IllegalStateException("Kan inte ta ut element - Kön är tom");
		}
		E returnElement = elements[lastIndex];
		elements[lastIndex] = null;
		lastIndex--;
		return returnElement;
	}
	//Tar ut det första elementet ur kön.
	public E takeFirst() {
		if (this.isEmpty()) {
			throw new IllegalStateException("Kan inte ta ut element - Kön är tom");
		}
		E returnElement = elements[0];
		for (int i = 0; i < lastIndex; i++) {
			elements[i] = elements[i+1];
		}
		lastIndex--;
		return returnElement;
	}
	private void enlarge() {
		//Bestäm storleken på den nya arrayen som lagrar kön
		int newLength = elements.length + (ENLARGEVALUE * elements.length) / 100;
		E[] newArray = (E[]) (new Object[newLength]);
		//Kopiera alla element från den gamla arrayen till en ny, större array
		for (int i = 0; i <= lastIndex; i++) {
			newArray[i] = elements[i];
		}
		//Sätt den nya arrayen att vara köns lagringsstruktur
		elements = newArray;
	}
	public static void main(String[] arg) {
		lifoQueue queue = new lifoQueue();
		System.out.println("Kön är tom? " + queue.isEmpty()); //true
		System.out.println("Kön är full? " + queue.isFull()); //false
//		queue.take(); //Testa exception
		queue.put(10);
		System.out.println("Kön är tom? " + queue.isEmpty()); //false
		System.out.println("Nästa element " + queue.peek()); //10
		queue.put(5);
		System.out.println("Nästa element " + queue.peek()); //5
		queue.put(2);
		queue.put(3);
		System.out.println("Nästa element " + queue.peek()); //3
		System.out.println("Tar ut  " + queue.take());
		System.out.println("Nästa element " + queue.peek()); //2
		System.out.println();

	}
}