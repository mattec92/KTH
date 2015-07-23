import java.io.*;
import java.util.*;
import java.lang.reflect.Array;

//Nodelement - lagrar nyckel och value, samt länk till nästföljande nod.
class Node<E,T>  {
	E key;
	T value;
	Node nextNode;
	public Node(E keyin, T valuein) {
		key = keyin;
		value = valuein;
		nextNode = null;
	}
	public String toString(){
		return (String) key + ":"+ value;
	}
}
public class HashTable<E, T> {
	//Defaultcomparator - för att kunna jämföra noder med varandra.
	//Nodens värde jämförs.
	private class DefaultComparator implements Comparator<Node> {
        public int compare (Node element1, Node element2) {
            int    compareValue = 0;
			try {
				Comparable element = (Comparable) element1.value;
				compareValue = element.compareTo(element2.value);
			}
			catch (ClassCastException e){
				throw new ClassCastException();
			}
			return compareValue;
		}
    };

    Comparator comp;
	Node[] table;
	int size;
	int count;
	//Konstruktor - skapar tabell med rätt storlek.
	public HashTable(int size) {
		this.size = size;
		comp = new DefaultComparator();
		//Skapar en array med generiska element.
		table = (Node[]) Array.newInstance(Node.class, size);
	}
	//contains - returnerar true om ett element med given nyckel finns.
	public boolean contains(E key) {
		int hash = Math.abs(key.hashCode() % size);
		Node currentNode = table[hash];
		//Går igenom nodsekvensen på framräknad plats.
		while (currentNode != null) {
			//Om element med rätt nyckel påträffas, returnera true.
			if (table[hash].key.equals(key)) {
				return true;
			}
			//Annars flytta currentpekaren till nästa nod.
			else {
				currentNode = currentNode.nextNode;
			}
		}
		//Om inget element med nyckeln finns, returnera false;
		return false;
	}
	//Get - retrunerar value för nod med sökt nyckel.
	public T get(E key) {
		T returnValue = null;
		//Räknar ut ett hashvärde, index i arrayen som noderna lagras i.
		int hash = Math.abs(key.hashCode() % size);
		Node currentNode = table[hash];
		//Går igenom nodsekvensen.
		while (currentNode != null) {
			//Om element med rätt nyckel påträffas, sätt returnValue till värdet och bryt loopen.
			if (currentNode.key.equals(key)) {
				returnValue = (T) currentNode.value;
				break;
			}
			//Annars flytta currentpekaren till nästa nod.
			else {
				currentNode = currentNode.nextNode;
			}
		}
		//Returnera returnValue.
		return returnValue;
	}
	//Put - lägger till ett element till hashtabellen
	public void put(E key, T value) {
		//Räknar ut ett hashvärde, index i arrayen som noderna lagras i.
		int hash = Math.abs(key.hashCode() % size);
		Node currentNode = table[hash];
		Node parentNode = null;
		boolean nodeFound = false;
		//Om det inte finns något element på platsen i arrayen, skapa och lägg till.
		if (currentNode == null) {
			table[hash] = new Node(key, value);
			count++;
		}
		//Annars gå igenom alla noder på platsen i arrayen.
		else {
			while (currentNode != null) {
				//Om nyckeln är samma som elementets nyckel.
				if ((currentNode.key).equals(key)) {
					//Byt ut det gamla värdet mot det nya.
					currentNode.value = value;
					return;
				}
				//Om fel nyckel, gå till nästa nod.
				else {
					parentNode = currentNode;
					currentNode = currentNode.nextNode;
				}
			}
			//Om det inte finns en nod med rätt nyckel, skapa en ny nod och lägg den först i nodsekvensen.
				Node newNode = new Node(key, value);
				newNode.nextNode = table[hash];
				table[hash] = newNode;
				count++;
		}
	}
	//remove - tar bort ett element med given nyckel ur hashtabellen.
	public void remove(E key) {
		int hash = Math.abs(key.hashCode() % size);
		Node currentNode = table[hash];
		Node parentNode = null;
		//Så länge det finns fler noder i tabellen på platsen.
		while (currentNode != null) {
			//Om nyckeln är samma som elementets nyckel.
			if (currentNode.key.equals(key)) {
				if (parentNode == null) {
					table[hash] = currentNode.nextNode;
				}
				else {
					parentNode.nextNode = currentNode.nextNode;
				}
				currentNode = null;
				count--;
				break;
			}
			//Om fel nyckel, gå till nästa nod.
			else {
				parentNode = currentNode;
				currentNode = currentNode.nextNode;
			}
		}
	}
	//toString - returnerar en stringrepresentation av hashtabellen
	public String toString() {
		String out = "";
		Node currentNode = null;
		//Går genom hashtabellens alla index.
		for (int i = 0; i < table.length; i++) {
			currentNode = table[i];
			//currentNode börjar peka på första elementet. Finns det fler element flyttas den till nästa element.
			//Skriver sedan informationen till en String.
			while (currentNode != null) {
				out += i + ": " + (String) currentNode.key + ": " + currentNode.value + "\n";
				currentNode = currentNode.nextNode;
			}
		}
		return out;
	}
	//toFile - skriver hashtabellens innehåll till en fil.
	public void toFile() throws IOException {
		BufferedWriter writer = new BufferedWriter(new FileWriter("frequency.txt"));
		Node currentNode = null;
		//Går genom hashtabellens alla index.
		for (int i = 0; i < table.length; i++) {
			currentNode = table[i];
			//currentNode börjar peka på första elementet. Finns det fler element flyttas den till nästa element.
			//Skriver sedan informationen i varje nod till en egen rad i en fil.
			while (currentNode != null) {
				writer.write((String) currentNode.key + ": " + currentNode.value);
				writer.newLine();
				currentNode = currentNode.nextNode;
			}
		}
		writer.close();
	}
	//toFileSorted - skriver hashtabellens innehåll till en fil.
	public void toFileSorted() throws IOException {
		BufferedWriter writer = new BufferedWriter(new FileWriter("frequency.txt"));
		Node[] nodes = this.toArray();
		Arrays.sort(nodes, comp);
		//Går genom hashtabellens alla index.
		for (int i = 0; i < nodes.length; i++) {
			//currentNode börjar peka på första elementet. Finns det fler element flyttas den till nästa element.
			//Skriver sedan informationen i varje nod till en egen rad i en fil.
				writer.write((String) nodes[i].key + ": " + nodes[i].value);
				writer.newLine();
		}
		writer.close();
	}
	//mostFrequentToFile - skriver de 100 mest förekommande orden till en fil.
	public void mostFrequentToFile() throws IOException {
		BufferedWriter writer = new BufferedWriter(new FileWriter("frequency.txt"));
		Node[] nodes = sortedArray();
		//Från den sorterade arrayen, ta elementen bakifrån och skriv till filen.
		for (int i = nodes.length-1; i > nodes.length - 101; i--) {
			writer.write((String) nodes[i].key+ ": " + nodes[i].value);
			writer.newLine();
		}
		writer.close();
	}
	//toArray - returnerar en array-representation med hashtabellens element.
	public Node[] toArray() {
		Node[] nodes = (Node[]) Array.newInstance(Node.class, count);
		Node currentNode = null;
		int arrayIndex = 0;
		//Går genom hashtabellens alla index.
		for (int i = 0; i < table.length; i++) {
			currentNode = table[i];
			//currentNode börjar peka på första elementet. Finns det fler element flyttas den till nästa element.
			//Skriver sedan noden till arrayen av noder.
			while (currentNode != null) {
				nodes[arrayIndex] = currentNode;
				currentNode = currentNode.nextNode;
				arrayIndex++;
			}
		}
		return nodes;
	}
	//sortedArray - returnerar en sorterad array-representation med hashtabellens element.
	public Node[] sortedArray() {
		Node[] nodes = this.toArray();
		Arrays.sort(nodes, comp);
		return nodes;
	}
}