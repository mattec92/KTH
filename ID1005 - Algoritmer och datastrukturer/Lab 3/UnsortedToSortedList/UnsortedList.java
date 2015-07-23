import java.util.*;

public class UnsortedList<E> {
	//Nodeelementet som listan är uppbyggd av.
	protected class Node {
		//Nodens element, samt pekare till nästa nod.
		public E element;
		public Node nextNode;

		public Node(E element) {
			this.element = element;
			this.nextNode = null;
		}
	}
	//Defaultcomparator - för att kunna jämföra element.
	protected class DefaultComparator implements Comparator<E> {
        public int compare (E element1, E element2) {
            int    compareValue = 0;
			try {
				Comparable<? super E> element = (Comparable<? super E>) element1;
				compareValue = element.compareTo(element2);
			}
			catch (ClassCastException e){
				throw new ClassCastException();
			}
			return compareValue;
		}
    };
	//Listans/trädets första element.
	protected Node firstNode;
	protected int numberOfElements;
	protected Comparator comparator = new DefaultComparator();

	//Konstruktor, skapar en tom lista.
	public UnsortedList() {
		firstNode = null;
	}
	//Size - returnerar antalet element i listan.
	public int size() {
		return numberOfElements;
	}
	//Metoden contains - Undersöker om ett element finns i listan.
	public boolean contains(E element) {
		//CurrentNode börjar i första noden.
		Node currentNode = firstNode;
		int compValue;
		//Körs så länge currentNode inte är null. Annars returna false.
		while (currentNode != null) {
			compValue = comparator.compare(element, currentNode.element);
			//Om elementet är likadant som ett i listan, returnera true,
			if (compValue == 0) {
				return true;
			}
			//Annars om det inte är likadant, undersök nästa element.
			else {
				currentNode = currentNode.nextNode;
			}
		}
		return false;
	}
	//Add - lägger till ett element i listan.
	public void add(E element) {
		//Skapa en ny nod innehållande det nya elementet.
		Node newNode = new Node(element);
		//newNodes nextNode sätts att peka på firstNode
		newNode.nextNode = firstNode;
		//firstNode sätts sedan att peka på den nya noden, newNode.
		firstNode = newNode;
		//Ökar antalet element med 1.
		numberOfElements++;
	}
	//remove - Tar bort ett element ur listan.
	public void remove(E element) {
		Node currentNode = firstNode;
		Node parentNode = null;
		int compValue;
		//Körs så länge currentNode inte är null. Annars returna false.
		while (currentNode != null) {
			compValue = comparator.compare(element, currentNode.element);
			//Om elementet är likadant som ett i listan, returnera true,
			if (compValue == 0) {
				break;
			}
			//Annars om det inte är likadant, undersök nästa element.
			else {
				parentNode = currentNode;
				currentNode = currentNode.nextNode;
			}
		}
		//Om den hittat ett element, alltså om while-loopen inte gått så att nextNode blir null.
		if (currentNode != null) {
			//ParentNode till noden som ska tas bort sätts till att peka på
			//den nod som tas borts nextNode.
			parentNode.nextNode = currentNode.nextNode;
		}
		numberOfElements--;
	}
	//Tostring - returnerar en stringrepresentation av listan.
	public String toString() {
		String out = "";
		Node currentNode = firstNode;
		//Så länge det finns fler element, lägg till dem till stringen out.
		while (currentNode != null) {
			out += currentNode.element + " ";
			currentNode = currentNode.nextNode;
		}
		return out;
	}

	//Representerar en iterator till listan. Möjliggör iterering.
	private class JIterator implements Iterator<E> {
		//Referens till nästa nod att returneras
		private Node currentNode;
		//Referens till senaste elmentet som returnerats
		private Node lastReturnedNode;

		//Skapar en iterator för att iterera genom listan en gång
		public JIterator() {
			//Sätter nextNode till first node och lastReturnedIndex, dvs parent till null.
			currentNode = firstNode;
			lastReturnedNode = null;
		}
		//Metoden hasnext, returnerar true om det finns ett nästa element.
		public boolean hasNext() {
			return currentNode != null;
		}
		//Metoden next - returnerar nästa element
		public E next() {
			//Om det inte finns ett nästa element, kasta exception
			if (!this.hasNext()) {
				throw new NullPointerException("End of iteration");
			}
			//Hämtar nästa element ur mängden.
			E element = currentNode.element;
			//Sätter lastReturnedIndex att peka på index till det element som ska returneras.
			lastReturnedNode = currentNode;
			//Sätt nextNode att peka på nästkommane nod.
			currentNode = currentNode.nextNode;
			return element;
		}
		//Metoden remove - tar bort senast besökta element.
		public void remove() {
			if (lastReturnedNode == null) {
				throw new IllegalStateException("Cannot remove - yet no element to remove");
			}
			UnsortedList.this.remove(lastReturnedNode.element);
			lastReturnedNode = null;
		}
	}
	//Returnerar en iterator för att iterera över mängden.
	public Iterator<E> iterator ()
	{
		return new JIterator ();
	}
	public static void main(String[] arg) {
//		UnsortedList<Integer> l = new UnsortedList<Integer>();
		SortedList<Integer> l = new SortedList<Integer>();
		System.out.println("Listans storlek: " + l.size());
		l.add(5);
		l.add(2);
		l.add(6);
		l.add(4);
		l.add(7);
		l.add(4);
		l.add(6);
		System.out.println("Listans storlek: " + l.size());
		System.out.println("Listans innehåll: " + l);
		l.remove(4);
		l.remove(5);
		System.out.println("Tar bort 4 och 5, listans innehåll efteråt: " + l);
		Iterator<Integer> iterator = l.iterator();
		System.out.print("Iteration genom listan: ");
		while (iterator.hasNext()) {
		    System.out.print(iterator.next() + " ");
		}
	}
}