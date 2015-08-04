import java.util.*;

public class SortedList<E> extends UnsortedList<E> {
	public SortedList() {
		super();
	}
	//Skapar en sorterad lista med samma element som given osorterad lista.
	public SortedList(UnsortedList<E> unsorted) {
		//Iterarar genom mängden och lägger till elementen.
		Iterator<E> iterator = unsorted.iterator();
		while (iterator.hasNext()) {
			this.add(iterator.next());
		}
	}

	public void add(E element) {
		//CurrentNode börjar i första noden.
		Node newNode = new Node(element);
		Node currentNode = firstNode;
		Node parentNode = null;
		int compValue;
		//Körs så länge currentNode inte är null, eller till villkor för stopp.
		while (currentNode != null) {
			compValue = comparator.compare(element, currentNode.element);
			//Om elementet är mindre eller lika som currentNodens element har du hittat rätt plats
			if (compValue <= 0) {
				break;
			}
			//Annars, undersök nästa position i listan.
			else {
				parentNode = currentNode;
				currentNode = currentNode.nextNode;
			}
		}
		//Om parentNode är null, dvs om elementet ska ligga först i listan.
		if (parentNode == null) {
			//Sätt firstNode att peka på den nya noden.
			firstNode = newNode;
			//newNodes nextNode sätts att peka på currentNode, förra förstapositionen i listan.
			newNode.nextNode = currentNode;
		}
		//Annars placera elementet på rätt plats i listan genom att sätta newNodes nextnode att
		//peka på currentNode, och att parentNodes nextNode att peka på den nya noden.
		else {
			newNode.nextNode = currentNode;
			parentNode.nextNode = newNode;
		}
		numberOfElements++;
	}
}