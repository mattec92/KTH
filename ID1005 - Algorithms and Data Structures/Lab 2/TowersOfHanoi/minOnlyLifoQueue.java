import java.util.*;

//Last in First out - kö där endast tal mindre än alla i kön kan läggas till.
public class minOnlyLifoQueue<E> {
	private E[] elements;
	private int lastIndex = -1;
	final int DEFAULT_CAPACITY = 100;
	final int ENLARGEVALUE = 50;
	private Comparator<E> comparator;
	//Standardcomparator för att jämföra element vid tilläggnig i kön.
	private class Comp implements Comparator<E> {
		public int compare (E element1, E element2) {
			Comparable<E> element = (Comparable<E>) element1;
			return element.compareTo(element2);
		}
	}

	//Skapar en ny kö med DEFAULT_CAPACITY (100) som storlek.
	public minOnlyLifoQueue() {
		elements = (E[]) (new Object[DEFAULT_CAPACITY]);
		comparator = new Comp();
	}
	//Returnerar hur många element som finns i kön.
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
	//Lägger till ett element i kön,  i denna implementation: sist.
	public void put(E element) {
		//Om kön är full, utöka köns storlek.
		if (lastIndex == elements.length - 1) {
			this.enlarge();
		}
		//Om kön är tom, lägg till elementet.
		if (this.isEmpty()) {
			elements[lastIndex+1] = element;
			lastIndex++;
		}
		//Om kön inte är tom, kolla om elementet är mindre än det sista elementet. Om det nya elementet är mindre, lägg till.
		else if (comparator.compare(element, elements[lastIndex]) == -1) {
			elements[lastIndex+1] = element;
			lastIndex++;
		}
	}
	//Kollar på nästa element i kön, i denna implementation: sista elementet.
	public E peek() {
		return elements[lastIndex];
	}
	//Tar ut ett element ur kön,i denna implementation: sista elementet.
	public E take() {
		if (this.isEmpty()) {
			throw new IllegalStateException("Kan inte ta ut element - Kön är tom");
		}
		E returnElement = elements[lastIndex];
		elements[lastIndex] = null;
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
	//Main - skapar kö och testar.
	public static void main(String[] arg) {
		minOnlyLifoQueue queue = new minOnlyLifoQueue();
		System.out.println("Kön är tom? " + queue.isEmpty()); //true
		System.out.println("Kön är full? " + queue.isFull()); //false
//		queue.take(); //Testa exception
		queue.put(10);
		System.out.println("Kön är tom? " + queue.isEmpty()); //false
		System.out.println("Nästa element " + queue.peek()); //10
		queue.put(5);
		queue.put(2);
		queue.put(3);
		System.out.println("Nästa element " + queue.peek()); //2
		System.out.println("Tar ut  " + queue.take());
		System.out.println("Nästa element " + queue.peek()); //5
		System.out.println();

	}
}