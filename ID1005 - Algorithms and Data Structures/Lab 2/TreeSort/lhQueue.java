/*Komplexitet:
 *Size: Går igenom från nod till nod tills det påträffas right/leftNode som är null. O(n)
 *Put/take: Worst case, där alla element läggs till i sorterad ordning ger att loopen för att
 *hitta rätt position körs n gånger. WorstCase har alltså komplexiteten O(n)
 *Är trädet balanserat, är komplexiteten O(logn)
 *peek: Följer left eller rightNode tills den kommer till en node utan childs. Komplexitet samma som
 *put och take.
 *
 *Vinsten jämfört med att använda en sekvensiell struktur är att komplexiteten för en sådan
 *kö är O(n) då man behöver gå igenom hela kön för att hitta det största respektive minsta elementet.
 *
 *Heapen ser bara till att det översta elementet är störst. TakeHighest hade hamnat på en motsvarande
 *komplexitet, men för att hitta minsta elementet är det binära sökträdet effektivare eftersom
 *det i heapen kan ligga var som helst, men som ett löv utan childs.
 *
 *
 */

import java.util.*;

public class lhQueue<E extends Comparable<? super E>> {
	//Nodeelementet som kön/trädet är uppbyggd av.
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
	//Kön/trädets root.
	private Node root;
	private int numberOfElements;

	public lhQueue() {
		root = null;
	}
	//Metoden isEmpty - om kön/trädet har en root, är kön inte tom.
	public boolean isEmpty() {
		return root == null;
	}
	//Metoden size - kallar på den rekursiva algoritmen som bestämmer storleken utifrån rooten.
	public int size() {
//		return size(root);
		return numberOfElements;
	}
	//Beräknar köns storlek. Går rekursivt från nod till nod tills den stöter på ett löv.
//	private int size(Node node) {
//		int count = 0;
//		if (node != null) {
//			count = 1 + size(node.leftNode) + size(node.rightNode);
//		}
//		return count;
//	}
	//Metoden put - Lägger till ett element i kön/trädet.
	public void put(E element) {
		//newNode är det element som ska placeras i kön/trädet.
		Node newNode = new Node(element);
		//Om kön är tom, placera elementet i rooten.
		if (root == null) {
			root = newNode;
		}
		//Annars, leta upp rätt plats i kön/trädet.
		else {
			Node currentNode = root;
			//Körs tills loopen bryts inifrån, när ett element har placerats.
			while (true) {
				int compare = element.compareTo(currentNode.element);
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
	//Metoden peekHighest - returnerar det största elementet i kön.
	public E peekHighest() {
		//currentNode sätts till köns root.
		Node currentNode = root;
		//Om rooten inte är null.
		if (root != null) {
			//Så länge det finns en rightNode, sätt currentNode till rightNode.
			//När det inte längre finns en rightNode, har det största elementet hittats.
			while (currentNode.rightNode != null) {
				currentNode = currentNode.rightNode;
			}
		}
		return currentNode.element;
	}
	//Metoden peekLowest - returnerar det minsta elementet i kön. Som peekHighest, fast går åt vänster.
	public E peekLowest() {
		Node currentNode = root;
		if (root != null) {
			//Så länge det finns en leftNode, sätt currentNode till rightNode.
			//När det inte längre finns en leftNode, har det minsta elementet hittats.
			while (currentNode.leftNode != null) {
				currentNode = currentNode.leftNode;
			}
		}
		return currentNode.element;
	}
	//Metoden takeHighest - returnerar det högsta elementet i kön/trädet. Tar även bort elementet.
	public E takeHighest() {
		Node currentNode = root;
		Node parentNode = null;
		//Om rooten inte är null, alltså om kön inte är tom.
		if (root != null) {
			//Så länge det finns en rightNode, sätt currentNode till rightNode.
			//När det inte längre finns en rightNode, har det största elementet hittats.
			while (currentNode.rightNode != null) {
				//parentNode sätts till currentNode och currentNode ändras att peka på nästa node.
				parentNode = currentNode;
				currentNode = currentNode.rightNode;
			}
		}
		//Elementet som ska returneras är currentNodes element när man hittat största elementet.
		E returnElement = currentNode.element;
		//Om elementet som ska returneras samt tas bort har en leftNode ska parentNode sättas att
		//peka på currentNodes leftNode. Största talet är alltid en rightNode, därför behöver
		//vi ej kolla om currentNode har en rightNode.
		//Om currentNode har en leftNode.
		if (currentNode.leftNode != null) {
			//Om currentNode är root till kön, sätt currentNodes leftNode till ny root.
			if (currentNode == root) {
				root = currentNode.leftNode;
			}
			//Annars sätt parentNodes rightNode som förrut pekade till currentNode,
			//till att peka på currentNodes leftNode.
			else {
				parentNode.rightNode = currentNode.leftNode;
			}
		}
		//Om rooten inte har någon leftNode eller rightNode, är elementet som ska returnas
		//rooten själv. Därav sätts rooten till null, och kön är nu tom.
		else if (root.leftNode == null && root.rightNode == null) {
			root = null;
		}
		//Annars, om det inte fanns någon leftNode till currentNode, kopplas bara currentNode av.
		else {
			parentNode.rightNode = null;
		}
		numberOfElements--;
		return returnElement;
	}

	//Metoden takeLowest - returnerar det lägsta elementet i kön/trädet. Tar även bort elementet.
	public E takeLowest() {
		Node currentNode = root;
		Node parentNode = null;
		//Om rooten inte är null, alltså om kön inte är tom.
		if (root != null) {
			//Så länge det finns en leftNode, sätt currentNode till leftNode.
			//När det inte längre finns en leftNode, har det minsta elementet hittats.
			while (currentNode.leftNode != null) {
				//parentNode sätts till currentNode och currentNode ändras att peka på nästa node.
				parentNode = currentNode;
				currentNode = currentNode.leftNode;
			}
		}
		//Elementet som ska returneras är currentNodes element när man hittat minsta elementet.
		E returnElement = currentNode.element;
		//Om elementet som ska returneras samt tas bort har en rightNode ska parentNode sättas att
		//peka på currentNodes rightNode. Minsta talet är alltid en leftNode, därför behöver
		//vi ej kolla om currentNode har en leftNode.
		//Om currentNode har en rightNode.
		if (currentNode.rightNode != null) {
			//Om currentNode är root till kön, sätt currentNodes rightNode till ny root.
			if (currentNode == root) {
				root = currentNode.rightNode;
			}
			//Annars sätt parentNodes leftNode som förrut pekade till currentNode,
			//till att peka på currentNodes rightNode.
			else {
				parentNode.leftNode = currentNode.rightNode;
			}
		}
		//Om rooten inte har någon leftNode eller rightNode, är elementet som ska returnas
		//rooten själv. Därav sätts rooten till null, och kön är nu tom.
		else if (root.leftNode == null && root.rightNode == null) {
			root = null;
		}
		//Annars, om det inte fanns någon rightNode till currentNode, kopplas bara currentNode av.
		else {
			parentNode.leftNode = null;
		}
		numberOfElements--;
		return returnElement;
	}
//	public String toString ()
//	{
//		StringBuilder    string = new StringBuilder ("");
//		toString (root, string);
//
//		return string.toString ();
//	}
//
//
//	// toString lägger till de element som ligger i ett givet träd
//	// till en given sträng. Trädets element läggs till enligt
//	// "inorder" ordningen.
//	private void toString (Node tree, StringBuilder string)
//	{
//		if (tree != null)
//		{
//            toString (tree.leftNode, string);
//			string.append (tree.element + " ");
//            toString (tree.rightNode, string);
//	    }
//	}

	public static void main(String[] arg) {
		lhQueue<Integer> q = new lhQueue<Integer>();
		Random rand = new Random();
		//Skapar en array med random tal.
		for (int i = 0; i < 20; i++) {
			q.put(Math.abs(rand.nextInt()) % 100);
		}
//		q.put(5);
//		q.put(3);
//		q.put(7);
//		System.out.println(q.peekHighest()); //7
//		System.out.println(q.peekLowest()); //3
//		q.put(2);
//		System.out.println(q.peekHighest()); // 7
//		System.out.println(q.peekLowest()); // 2
//		System.out.println(q.takeHighest()); //7
//		System.out.println(q.takeHighest()); //5
//		System.out.println(q.takeHighest()); //3
//		q.put(4);
//		q.put(1);
//		System.out.println(q.takeHighest()); //4
//		System.out.println(q.peekHighest()); //2
//		System.out.println(q.takeLowest()); //1
//		System.out.println(q.takeLowest()); //2
	}
}