import java.util.*;

public class BinaryTreeMultiset<E> implements Set<E>  {
	//Nodeelementet som mängden/trädet är uppbyggd av.
	private class Node {
		//Nodens element, samt pekare till left- och rightNode
		public E element;
		public Node leftNode;
		public Node rightNode;

		public Node(E element) {
			this.element = element;
			this.leftNode = null;
			this.rightNode = null;
		}
	}
	//Defaultcomparator - för att kunna jämföra element.
	 private class DefaultComparator implements Comparator<E> {
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


	//Mängdens/trädets root.
	private Node root;
	private int numberOfElements;
	private Comparator comparator = new DefaultComparator();

	//Konstruktor för en tom mängd.
	public BinaryTreeMultiset() {
		root = null;
	}
	//Konstruktor för en ny mängd som innehåller samma element som given mängd.
	public BinaryTreeMultiset(Set<E> set) {
		root = null;
		for(E element : set) {
			add(element);
		}
	}
	//Konstruktor för en mängd som innehåller alla element som specificerats i en array.
//	public BinaryTreeMultiset(E[] elements) {
//		root = null;
//		for(E element : elements) {
//			add(element);
//		}
//	}
	//Metoden isEmpty - om mängden/trädet har en root, är mängden inte tom.
	public boolean isEmpty() {
		return root == null;
	}
	//Metoden size - returnerar antalet element i mängden.
	public int size() {
		return numberOfElements;
	}
	//Metoden add - lägger till ett element till mängden.
	public void add(E element) {
		//newNode är det element som ska placeras i trädet.
		Node newNode = new Node(element);
		//Om mängden inte redan innehåller elementet man vill lägga till.
		//Om mängden är tom, placera elementet i rooten.
		if (root == null) {
			root = newNode;
		}
		//Annars, leta upp rätt plats i trädet.
		else {
			Node currentNode = root;
			//Körs tills loopen bryts inifrån, när ett element har placerats.
			while (true) {
				int compare = comparator.compare(element, currentNode.element);
				//Om elementet är mindre än currentNodes element.
				if (compare <= 0) {
					//Om currentNode inte har en leftNode, sätt currentNodes leftNode att peka på newNode.
					if (currentNode.leftNode == null) {
						currentNode.leftNode = newNode;
						break;
					}
					//Annars flytta currentpekaren till currentNodes leftNode.
					else {
						currentNode = currentNode.leftNode;
					}
				}
				//Om elementet är större än currentNodes element.
				else {
					//Om currentNode inte har en rightNode.
					if (currentNode.rightNode == null) {
						currentNode.rightNode = newNode;
						break;
					}
					//Annars flytta currentpekaren till rootens rightNode.
					else {
						currentNode = currentNode.rightNode;
					}
				}
			}
		}
		numberOfElements++;
	}
	//Metoden contains - Undersöker om ett element finns i mängden.
	public boolean contains(E element) {
		//CurrentNode börjar i rooten.
		Node currentNode = root;
		int compValue;
		//Körs så länge currentNode inte är null. Annars returna false.
		while (currentNode != null) {
			compValue = comparator.compare(element, currentNode.element);
			//Om elementet är likadant som ett i mängden, returnera true,
			if (compValue == 0) {
				return true;
			}
			//Annars om det sökta elementet är större, gå till currentNodes rightNode.
			else if (compValue == 1) {
				currentNode = currentNode.rightNode;
			}
			//Annars om det sökta elementet är mindre, gå till currentNodes leftNode.
			else {
				currentNode = currentNode.leftNode;
			}
		}
		return false;
	}
	//Metoden remove - tar bort ett element ur mängden.
	public void remove(E element) {
		Node currentNode = root;
		Node parent = null;
		int compValue;
		boolean elementFound = false;
		//Körs så länge currentNode inte är null.
		while (currentNode != null) {
			compValue = comparator.compare(element, currentNode.element);
			//Om elementet är likadant som ett i mängden, break.
			if (compValue == 0) {
				elementFound = true;
				break;
			}
			//Annars om det sökta elementet är större, gå till currentNodes rightNode.
			else if (compValue == 1) {
				parent = currentNode;
				currentNode = currentNode.rightNode;
			}
			//Annars om det sökta elementet är mindre, gå till currentNodes leftNode.
			else {
				parent = currentNode;
				currentNode = currentNode.leftNode;
			}
		}
		//Om man har hittat ett likadant element.
		if (elementFound) {
			//Om elementet ligger i rooten
			if (currentNode == root) {
				//Om rooten inte har någon leftNode, sätts rooten till rootens rightNode.
				if (root.leftNode == null) {
					root = root.rightNode;
				}
				//Annars hitta rätt Node att sätta i rooten
				else {
					Node tempNode = root.leftNode;
					Node tempParent = root;
					//Hitta det största elementet som är mindre än rooten.
					while (tempNode.rightNode != null) {
						tempParent = tempNode;
						tempNode = tempNode.rightNode;
					}
					//Om elementets parent är rooten, sätt elementet till ny root,
					//och elementets rightNode sätts till den gamla rootens rightNode.
					if (tempParent == root) {
						tempNode.rightNode = root.rightNode;
						root = tempNode;
					}
					//Annars byt ut rootens element mot elementet, och sätt
					//parents rightNode att peka på nodens leftNode.
					else {
						root.element = tempNode.element;
						tempParent.rightNode = tempNode.leftNode;
					}
				}
			}
			//Om noden bara har ett barn, och det är åt höger
			else if (currentNode.leftNode == null) {
				//Om parents leftNode är currentNode, ska parents leftNode sättas till currentNodes rightNode.
				if (parent.leftNode == currentNode) {
					parent.leftNode = currentNode.rightNode;
				}
				//Annars ska parrents rightNode sättas till currentNodes rightNode.
				else {
					parent.rightNode = currentNode.rightNode;
				}
			}
			//Om noden bara har ett barn, och det är åt vänster
			else if (currentNode.rightNode == null) {
				//Om parents leftNode är currentNode, ska parents leftNode sättas till currentNodes leftNode.
				if (parent.leftNode == currentNode) {
					parent.leftNode = currentNode.leftNode;
				}
				//Annars ska parrents rightNode sättas till currentNodes leftNode.
				else {
					parent.rightNode = currentNode.leftNode;
				}
			}
			//Om noden har två barn
			else {
				Node tempNode = currentNode.leftNode;
				Node tempParent = currentNode;
				//Hitta det största elementet som är mindre än rooten.
				while (tempNode.rightNode != null) {
					tempParent = tempNode;
					tempNode = tempNode.rightNode;
				}
				//Flytta den funna nodens element till noden som ska tas bort.
				currentNode.element = tempNode.element;
				//Om parent är samma som current, sätt parents leftNode till den funna leftNode
				if (tempParent == currentNode) {
					tempParent.leftNode = tempNode.leftNode;
				}
				//Annars sätt parents rightNode till den funna leftNode
				else {
					tempParent.rightNode = tempNode.leftNode;
				}
			}
			numberOfElements--;
		}
	}
	//Metoden clear - kopplar bort trädet
	public void clear() {
		root = null;
	}
	//isSubsetOf - returnerar true om mängden är en delmängd av given mängd.
	public boolean isSubsetOf (Set<E> set) {
		for (E element : this) {
			//Går igenom alla element och kollar om de finns i den andra mängden.
			//Om något element inte finns med, returnera false.
			if (!set.contains(element)) {
				return false;
			}
		}
		return true;
	}
	//union - returnerar en union av två mängder.
	public BinaryTreeMultiset<E> union (Set<E> set) {
		BinaryTreeMultiset<E> setout = new BinaryTreeMultiset<E>(this);
		//Går igenom alla element och lägger till i den nya mängden som redan innehåller ena mängden.
		for (E element : set) {
				setout.add(element);
		}
		return setout;
	}
	//intersection - returnerar snittet av två mängder
	public BinaryTreeMultiset<E> intersection(Set<E> set) {
		BinaryTreeMultiset<E> setout = new BinaryTreeMultiset<E>();
		//Går genom hela mängden och kollar om elementet finns i den andra mängden.
		//Isåfall, lägg till elementet till en ny mängd som returneras.
		for (E element : this) {
			if (set.contains(element)) {
				setout.add(element);
			}
		}
		return setout;
	}
	public BinaryTreeMultiset<E> difference (Set<E> set) {
		BinaryTreeMultiset<E> setout = new BinaryTreeMultiset<E>();
		//Går genom hela mängden och kollar om elementet finns i den andra mängden.
		//Om det inte finns, lägg till elementet till en ny mängd som returneras.
		for (E element : this) {
			if (!set.contains(element)) {
				setout.add(element);
			}
		}
		return setout;
	}
	//toString - returnerar en stringrepresentation av mängden
	public String toString() {
		StringBuilder string = new StringBuilder("");
		toString(root, string);
		return string.toString();
	}
	//Rekursiv metod som bygger upp stringrepresentationen.
	private void toString(Node node, StringBuilder string) {
		if (node != null) {
			toString(node.leftNode, string);
			string.append(node.element + " ");
			toString(node.rightNode, string);
		}
	}
	int pos = 0;
	//toArray - returnerar en array-representation av mängden i stigande ordning.
	public E[] toArray() {
		E[] array = (E[]) new Object[numberOfElements];
		toArray(root, array);
		pos = 0;
		return array;
	}
	//Hjälpmetod, lägger rekursivt till alla element på rätt plats i arrayen.
	private void toArray(Node node, E[] array) {
		if (node != null) {
			//Lägger till rootens vänstra barn.
			toArray(node.leftNode, array);
			array[pos] = node.element;
			pos++;
			//Lägger till rootens högra barn.
			toArray(node.rightNode, array);
		}
	}
	//Representerar en iterator till mängden. Möjliggör iterering.
	private class JIterator implements Iterator<E> {
		private E[] elements = null;
		//Index för nästa element
		private int nextIndex = -1;
		//Index för det senaste elmentet som returnerats
		private int lastReturnedIndex;

		//Skapar en iterator för att iterera genom mängden en gång
		public JIterator() {
			elements = BinaryTreeMultiset.this.toArray();
			//Sätter nextIndex till 0 om det finns element, annars till -1.
			nextIndex = (elements.length != 0) ? 0 : -1;
			lastReturnedIndex = -1;
		}
		//Metoden hasnext, returnerar true om det finns ett nästa element.
		public boolean hasNext() {
			return nextIndex != -1;
		}
		//Metoden next - returnerar nästa element
		public E next() {
			//Om det inte finns ett nästa element, kasta exception
			if (!this.hasNext()) {
				throw new NullPointerException("End of iteration");
			}
			//Hämtar nästa element ur mängden.
			E element = elements[nextIndex];
			//Sätter lastReturnedIndex att peka på index till det element som ska returneras.
			lastReturnedIndex = nextIndex;
			//Om det finns ett nästa element, sätt nextIndex att peka på nästkommane index.
			if (nextIndex < elements.length - 1) {
				nextIndex++;
			}
			//Annars sätt nextIndex till -1 för att markera att det inte finns fler element att iterera över.
			else {
				nextIndex = -1;
			}
			return element;
		}
		//Metoden remove - tar bort senast besökta element.
		public void remove() {
			if (lastReturnedIndex == -1) {
				throw new IllegalStateException("Cannot remove - yet no element to remove");
			}
			BinaryTreeMultiset.this.remove(elements[lastReturnedIndex]);
			lastReturnedIndex = -1;
		}
	}
	//Returnerar en iterator för att iterera över mängden.
	public Iterator<E> iterator ()
	{
		return new JIterator ();
	}

	public static void main(String[] arg) {
		BinaryTreeMultiset<Integer> s = new BinaryTreeMultiset<Integer>();
		s.add(5);
		s.add(2);
		s.add(6);
		System.out.println("Adderat 5, 2, 6 till mängden S: " + s);
		System.out.println("Arrayrepresenation av S: " + Arrays.toString(s.toArray()));
		System.out.println("Size av S: " + s.size());
		s.add(2);
		System.out.println("Lagt till 2 till S, size av S: " + s.size());
		System.out.println("S: " + s);
		s.remove(2);
		System.out.println("Tagit bort 2 från S, size av S: " + s.size());
		System.out.println("S innehåller 2: " + s.contains(2));
		System.out.println("S innehåller 6: " + s.contains(6));
		Iterator<Integer> iterator = s.iterator();
		System.out.print("Iteration genom mängden: ");
		while (iterator.hasNext()) {
		    System.out.print(iterator.next () + " ");
		}
		BinaryTreeMultiset<Integer> d = new BinaryTreeMultiset<Integer>(s);
		d.add(7);
		System.out.println("\nSkapat en kopia av S och lagt till 7 i den");
		System.out.println("S: " + s);
		System.out.println("D: " + d);
		System.out.println("D subset av S: " + d.isSubsetOf(s));
		System.out.println("S subset av D: " + s.isSubsetOf(d));
		System.out.println("D union av S:" + d.union(s));
		System.out.println("D intersection av S: " + d.intersection(s));
		System.out.println("D difference av D: " + d.difference(s));
	}
}