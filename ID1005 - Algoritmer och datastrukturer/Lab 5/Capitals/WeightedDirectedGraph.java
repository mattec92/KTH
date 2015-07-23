import java.util.*;

public class WeightedDirectedGraph {
	//Node -  representerar en nod i en sekvens med grannhörn
    private static class Node implements Comparable<Node> {
		//Index av ett grannhörn i vektorn med hörn
		public int fromIndex;
		public int toIndex;
		public int edgeWeight;
		//Nästa nod i sekvensen med grannhörn
		public Node    nextNode;
		public Node (int toIndex, int edgeWeight) {
			this.toIndex = toIndex;
			this.edgeWeight = edgeWeight;
			this.nextNode = null;
		}
		public int compareTo(Node compareNode) {
			if (edgeWeight < compareNode.edgeWeight) {
				return -1;
			}
			else if (edgeWeight == compareNode.edgeWeight) {
				return 0;
			}
			else {
				return 1;
			}
		}
		public String toString() {
			return "" + edgeWeight;
		}
	}
	public static final int    DEFAULTCAPACITY = 100;
	//Hur mycket kapaciteten ändras med vid enlarge. Här 25%.
    public static final int    ENLARGEVALUE = 25;
	//Grafens hörn
    private String[]    corners;
    //Sekvenser med vägar mellan hörn
    private Node[]    routes;
    private int    lastIndex = -1;

   	//Konstruktor - skapar en graf med default kapacitet.
    public WeightedDirectedGraph() {
        corners = new String[DEFAULTCAPACITY];
        routes = new Node[DEFAULTCAPACITY];
	}
    //Konstruktor - skapar en graf med given kapacitet.
    public WeightedDirectedGraph(int initialCapacity) {
        corners = new String[initialCapacity];
        routes = new Node[initialCapacity];
	}
	//Size - returnerar antalet hörn i grafen.
	public int size() {
		return lastIndex + 1;
	}
	//isEmpty - returnerar true om grafen är tom, annars false.
	public boolean isEmpty() {
		return lastIndex == -1;
	}
	//Enlarge - utökar grafens kapacitet.
	protected void enlarge() {
		//Utöka storleken med 25% och skapa nya arrayer med den nya storleken.
		int newLength = 1 + corners.length + ENLARGEVALUE * corners.length/100;
		String[] newCorners = new String[newLength];
		Node[] newRoutes = new Node[newLength];
		//Kopiera corners och routes till de nya arrayerna.
		for (int i = 0; i <= lastIndex; i++) {
			newCorners[i] = corners[i];
			newRoutes[i] = routes[i];
			corners[i] = null;
			routes[i] = null;
		}
		corners = newCorners;
		routes = newRoutes;
	}
	//indexOf - returnerar index av det hörn som är likadant som givet hörn.
	protected int indexOf(String corner) {
		//Gå igenom arrayen med hörn och jämför med givet hörn. Om lika, returnera index.
		for (int i = 0; i <= lastIndex; i++) {
			if (corner.equals(corners[i])) {
				return i;
			}
		}
		//Annars returnera -1.
		return -1;
	}
	//hasCorner - returnerar true om det finns ett hörn som är likadant som givet hörn.
	public boolean hasCorner(String corner) {
		return indexOf(corner) != -1;
	}
	//Lägger till ett hörn om det inte redan finns ett likadant hörn.
	public void addCorner(String corner) {
		if (this.hasCorner(corner) == false) {
			if (lastIndex == corners.length - 1) {
				enlarge();
			}
			lastIndex++;
			corners[lastIndex] = corner;
		}
	}
	//getCorners - returnerar en array med alla hörn.
	public String[] getCorners() {
		String[] allCorners = new String[lastIndex + 1];
		for (int i = 0; i < allCorners.length; i++) {
			allCorners[i] = corners[i];
		}
		return allCorners;
	}
	//getNeighbours - returnerar en array med hörnets grannhörn.
	public String[] getNeighbours(String corner) throws IllegalArgumentException {
		//Plats i hörnarrayen där hörnet finns.
		int index = indexOf(corner);
		//Om hörnet inte finns, kasta exception
		if (index < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + corner);
		}
		//Sätt currentNode att peka på första noden i nodsekvensen av routes.
		Node currentNode = routes[index];
		int count = 0;
		//Så länge det finns fler routes, räkna hur många det finns.
		while (currentNode != null) {
			count++;
			currentNode = currentNode.nextNode;
		}
		//Skapa en array med rätt antal platser.
		String[] out = new String[count];
		currentNode = routes[index];
		int neighbourIndex = 0;
		//Gå igenom nodsekvensen av routes igen och lägg till corners som
		//routsen leder till i arrayen med grannhörn som ska returneras.
		while(currentNode != null) {
			out[neighbourIndex++] = corners[currentNode.toIndex];
			currentNode = currentNode.nextNode;
		}
		return out;
	}
	//hasEdge - returnerar true om det finns en route mellan två givna hörn.
	//Observera att eftersom grafen är riktad räknas bara routes som går från
	//givet corner1 till corner2.
	public boolean hasRoute(String corner1, String corner2) throws IllegalArgumentException {
		//Hämtar hörnens index i arrayen med hörn.
		int index1 = indexOf(corner1);
		int index2 = indexOf(corner2);
		//Om de inte finns, kasta exception.
		if (index1 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + corner1);
		}
		if (index2 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + corner2);
		}
		//Sätt currentNode att peka på första routen corner1 har.
		Node currentNode = routes[index1];
		//Gå längs nodsekvensen och kolla om det finns en route till corner2's index.
		//Isåfall returnera true, annars löps loopen vidare och false returneras om
		//Ingen route mellan givna hörn hittats.
		while (currentNode != null) {
			if (currentNode.toIndex == index2) {
				return true;
			}
			else {
				currentNode = currentNode.nextNode;
			}
		}
		return false;
	}
	public int routeWeight(String corner1, String corner2) {
		//Hämtar hörnens index i arrayen med hörn.
		int index1 = indexOf(corner1);
		int index2 = indexOf(corner2);
		//Om de inte finns, kasta exception.
		if (index1 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + corner1);
		}
		if (index2 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + corner2);
		}
		//Sätt currentNode att peka på första routen corner1 har.
		Node currentNode = routes[index1];
		//Gå längs nodsekvensen och kolla om det finns en route till corner2's index.
		//Isåfall returnera vikten, annars löps loopen vidare och -1 returneras om
		//Ingen route mellan givna hörn hittats.
		while (currentNode != null) {
			if (currentNode.toIndex == index2) {
				return currentNode.edgeWeight;
			}
			else {
				currentNode = currentNode.nextNode;
			}
		}
		return -1;
	}
	//Lägger till en nod till den nodsekvens given av indexet.
	//Noden placeras så att sekvensen förblir sorterad efter vikter.
	protected void addNode(Node node, int index) {
		//Sätt currentNode att peka på första noden i sekvensen som utgår från givet hörnindex.
		Node currentNode = routes[index];
		//Om det inte finns några routes från hörnet, lägg till noden/routen.
		if (currentNode == null) {
			routes[index] = node;
		}
		else {
			Node previousNode = null;
			//Annars, så länge det finns fler noder i sekvensen och nuvarande nod
			//har lägre vikt än den som ska läggas till, gå längs nodsekvensen och
			//uppdatera currentNode och previousNode.
			while (currentNode != null && currentNode.edgeWeight < node.edgeWeight) {
				previousNode = currentNode;
				currentNode = currentNode.nextNode;
			}
			//Om previousNode är null, alltså om noden ska ligga först i sekvensen,
			//sätt routes[index] att peka på den nya noden.
			if (previousNode == null) {
				routes[index] = node;
			}
			//Annars om det inte är den första noden i sekvensen, sätt föregående nods
			//nextNode att peka på noden som ska läggas till.
			else {
				previousNode.nextNode = node;
			}
			//Sätt sedan den nya noden att peka på currentNode.
			node.nextNode = currentNode;
		}
	}
	//removeNode - tar bort den nod som symboliserar routen mellan två hörn.
	protected void removeNode(int from, int to) {
		//Sätt currentNode att peka på den första noden i given nodsekvens.
		Node currentNode = routes[from];
		Node previousNode = null;
		//Så länge det finns fler noder i sekvensen och rätt route inte är funnen.
		while (currentNode != null  &&  currentNode.toIndex != to) {
			previousNode = currentNode;
			currentNode = currentNode.nextNode;
		}
		//Om det finns någon route.
		if (currentNode != null) {
			//Om Noden som ska tas bort inte är den första, sätt previousNode
			//att peka på currentNodes nextNode.
			if (previousNode != null) {
				previousNode.nextNode = currentNode.nextNode;
			}
			//Annars sätt route-arrayens pekare på given positon att peka på
			//currentNodes nextNode.
			else {
				routes[from] = currentNode.nextNode;
			}
		}
	}
	//Lägger till en route mellan två hörn med given vikt.
	public void addRoute(String fromCorner, String toCorner, int weight) throws IllegalArgumentException {
		//Hämtar hörnens index i arrayen med hörn.
		int index1 = indexOf(fromCorner);
		int index2 = indexOf(toCorner);
		//Om de inte finns, kasta exception.
		if (index1 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + fromCorner);
		}
		if (index2 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + toCorner);
		}
		//Om det finns en route mellan givna hörn, ta bort den.
		if (hasRoute(fromCorner, toCorner)) {
			removeNode(index1, index2);
		}
		//Skapa ny nod med given riktning och vikt
		Node newNode = new Node(index2, weight);
		//Lägg till noden till given sekvens.
		addNode(newNode, index1);
	}
	//Tar bort routen mellan givna hörn.
	public void removeRoute(String fromCorner, String toCorner) throws IllegalArgumentException {
		int index1 = indexOf(fromCorner);
		int index2 = indexOf(toCorner);
		//Om de inte finns, kasta exception.
		if (index1 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + fromCorner);
		}
		if (index2 < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + toCorner);
		}
		//Kalla på removeNode som tar bort eventuella routes mellan givna hörn.
		removeNode(index1, index2);
	}
	//Tar bort alla routes från givet hörn.
	public void removeRoutes(String corner) throws IllegalArgumentException {
		int index = indexOf(corner);
		//Om hörnet inte finns, kasta exception.
		if (index < 0) {
			throw new IllegalArgumentException("Fel med hörnet: " + corner);
		}
		//Ta bort alla routes från givet hörn.
		routes[index] = null;
		//Gå igenom alla routes och hitta routes som går till givet hörn.
		for(int i = 0; i <= lastIndex; i++) {
			Node currentNode = routes[i];
			while (currentNode != null) {
				//Om route går till givet hörn, ta bort, annars gå vidare i nodsekvensen.
				if (currentNode.toIndex == index) {
					removeRoute(corners[i], corner);
				}
				currentNode = currentNode.nextNode;
			}
		}
	}
	//removeCorner - tar bort ett hörn i grafen som är likadant som givet hörn.
	public void removeCorner(String corner) {
		//Hämta hörnets plats i arrayen med hörn.
		int index = indexOf(corner);
		//Om hörnet finns
		if (index != -1) {
			//Ta bort alla routes till och från hörnet.
			removeRoutes(corner);
			//Ta bort hörnet och flytta innehållet i arrayerna.
			for (int i = index + 1; i <= lastIndex; i++) {
				corners[i - 1] = corners[i];
				routes[i - 1] = routes[i];
			}
			corners[lastIndex] = null;
			routes[lastIndex] = null;
			lastIndex--;
			//Uppdatera routes med nya toIndex.
			for (int i = 0; i <= lastIndex; i++) {
				Node currentNode = routes[i];
				//Gå genom nodsekvenserna.
				while (currentNode != null) {
					//Om toIndex är mindre än index av hörnet som togs bort har den flyttats.
					//Minska isåfall toIndex med 1 eftersom den flyttats en position längre fram i arrayen.
					if (currentNode.toIndex > index) {
						currentNode.toIndex--;
					}
					currentNode = currentNode.nextNode;
				}
			}
		}
	}
	//Clear - tar bort alla hörn och routes ifrån grafen.
	public void clear() {
		corners = new String[corners.length];
		routes = new Node[routes.length];
		lastIndex = -1;
	}
	//hasRouteBreadth - returnerar true om det finns en route mellan två hörn.
	public boolean hasRouteBreadth(String start, String stop) {
		ArrayDeque<String> q = new ArrayDeque<String>();
		String[] visited = new String[lastIndex+1];
		boolean found = false;
		String current = start;
		int count = 0;
		//Lägger till start-hörnet i kön.
		q.addLast(current);
		//Körs så länge man inte hittat en väg till rätt hörn.
		while (found == false) {
//			System.out.println(q);
			//Om kön är tom har man gått igenom alla hörn utan att hitta, avbryt och returnera false.
			if (q.isEmpty()) {
				break;
			}
			//Lägg köns första hörn till current, och lägg current till listan av besökta hörn.
			current = q.removeFirst();
			visited[count++] = current;
			//Om man befinner sig i rätt hörn, returnera true.
			if (current == stop) {
				found = true;
			}
			//Annars, gå igenom alla routes från current-hörnet
			else {
				Node currentNode = routes[indexOf(current)];
				//Körs så länge det finns fler routes från current hörnet.
				while (currentNode != null) {
					//Om routes från current-hörnet går till ett annat hörn som ej är besökt,
					//lägg till det i kön. Undersök sedan nästa route i sekvensen.
					if (Arrays.asList(visited).contains(corners[currentNode.toIndex]) == false) {
						q.addLast(corners[currentNode.toIndex]);
					}
					currentNode = currentNode.nextNode;
				}
			}
		}
		return found;
	}
	//hasRouteDepth - returnerar true om det finns en route mellan två hörn.
	public boolean hasRouteDepth(String start, String stop) {
		ArrayDeque<String> q = new ArrayDeque<String>();
		String[] visited = new String[lastIndex+1];
		boolean found = false;
		String current = start;
		int count = 0;
		//Lägger till start-hörnet i kön.
		q.addLast(current);
		//Körs så länge man inte hittat en väg till rätt hörn.
		while (found == false) {
//			System.out.println(q);
			//Om kön är tom har man gått igenom alla hörn utan att hitta, avbryt och returnera false.
			if (q.isEmpty()) {
				break;
			}
			//Lägg köns första hörn till current, och lägg current till listan av besökta hörn.
			current = q.removeFirst();
			visited[count++] = current;
			//Om man befinner sig i rätt hörn, returnera true.
			if (current == stop) {
				found = true;
			}
			//Annars, gå igenom alla routes från current-hörnet
			else {
				Node currentNode = routes[indexOf(current)];
				//Körs så länge det finns fler routes från current hörnet.
				while (currentNode != null) {
					//Om routes från current-hörnet går till ett annat hörn som ej är besökt,
					//lägg till det i kön. Undersök sedan nästa route i sekvensen.
					if (Arrays.asList(visited).contains(corners[currentNode.toIndex]) == false) {
						q.addLast(corners[currentNode.toIndex]);
					}
					currentNode = currentNode.nextNode;
				}
			}
		}
		return found;
	}
	//MST - returnerar ett minimalt spanning-tree till grafen utifrån Kruskals algoritm.
	public WeightedDirectedGraph MST() {
		WeightedDirectedGraph res = new WeightedDirectedGraph();
		//Lägg till alla hörn till den nya resultatgrafen.
		for (int i = 0; i < lastIndex+1; i++) {
			res.addCorner(corners[i]);
		}
		//Skapa en array som symboliserar vilka mängder hörnen tillhör.
		int[] sets = new int[lastIndex+1];
		int included = 0;
		//Lägg till alla hörn i olika sets.
		for (int i = 0; i < lastIndex+1; i++) {
			sets[i] = i;
		}
		LinkedList<Node> routeList = new LinkedList<Node>();
		//Lägg alla routes till en länkad lista.
		for (int i = 0; i <= lastIndex; i++) {
			Node currentNode = routes[i];
			while (currentNode != null) {
				//Utvidgar Noden genom att lägga till även start-hörn.
				currentNode.fromIndex = i;
				routeList.addLast(currentNode);
				currentNode = currentNode.nextNode;
			}
		}
//		System.out.println(routeList);
		//Sorterar listan så att routesen ligger i ordning där lägst weight ligger först.
		Collections.sort(routeList);
//		System.out.println(routeList);
		//Körs så länge antalet routes i resultatmängden är mindre än lastIndex, dvs antal corners -1
		while (included < lastIndex) {
			//Sätt node att peka på första routen,som även tas bort ur listan.
			Node node = routeList.removeFirst();
			//Om routen går mellan två olika mängder, lägg till routen till den nya grafen.
			if (sets[node.fromIndex] != sets[node.toIndex]) {
				res.addRoute(corners[node.fromIndex], corners[node.toIndex], node.edgeWeight);
				//Ändra i arrayen som representerar vilka mängder hörnen tillhör så
				//de två hörnen numera tillhör samma set.
				sets[node.fromIndex] = sets[node.toIndex];
				included++;
			}
		}
		return res;
	}
	//Dijkstras algoritm - bestämmer kortaste vägarna från ett hörn till alla andra hörn.
	//Returnerar sedan en graf innehållande endast de vägar som ger kortast väg samt total weight.
	public WeightedDirectedGraph Dijkstra(String source) {
		WeightedDirectedGraph g = new WeightedDirectedGraph();
		int[] distance = new int[lastIndex+1];
		boolean[] visited = new boolean[lastIndex+1];
		int currentMax = Integer.MAX_VALUE;
		int currentCorner = 0;
		//Lägg till alla hörn till den nya grafen.
		for (int m = 0; m <= lastIndex; m++) {
			g.addCorner(corners[m]);
		}
		//Lägg till högsta möjliga avstånd till hörnen.
		for (int i = 0; i < distance.length; i++) {
			distance[i] = Integer.MAX_VALUE;
		}
		//Sätt avståndet från source-hörnet att vara 0.
		distance[indexOf(source)] = 0;
		for (int i = 0; i < distance.length; i++) {
			//Hitta den route med kortast avstånd, detta hörn ska besökas.
			for (int k = 0; k < distance.length; k++) {
				if (visited[k] == false && distance[k] < currentMax) {
					currentCorner = k;
					currentMax = distance[k];
				}
			}
//			System.out.println("NEXT: " + currentCorner + Arrays.toString(distance));
			//Markera nuvarande hörn som besökt.
			visited[currentCorner] = true;
			//Hämta nuvarande hörns grannar.
			String[] neighbours = getNeighbours(corners[currentCorner]);
			//Gå igenom nuvarande hörnets grannar.
			for (int j = 0; j < neighbours.length; j++) {
				String currentNeighbour = neighbours[j];
				//Beräkna ny distans till nuvarande hörnets grannar via nuvarande hörnet.
				int newDistance = distance[currentCorner] + routeWeight(corners[currentCorner], currentNeighbour);
				//Om distansen via nuvarande hörnet är kortare än redan funnen vägs distans.
				if (distance[indexOf(currentNeighbour)] > newDistance) {
					//Lägg till routen som används till den nya grafen.
					g.addRoute(corners[currentCorner], currentNeighbour, routeWeight(corners[currentCorner], currentNeighbour));
					//Lägg till den nya distansen till arrayen med närmaste routes.
					distance[indexOf(currentNeighbour)] = newDistance;
				}
			}
			//Nollställ next och currentMax till sina startvärden för att hitta nästa kortaste väg.
			currentCorner = 0;
			currentMax = Integer.MAX_VALUE;
		}
		System.out.println(Arrays.toString(distance));
		System.out.println(Arrays.toString(visited));
		return g;
	}
	// toString returnerar grafens strängrepresentation
    public String toString ()
    {
		// sträng med hörn
		String    verticesString = "{";
		for (int index = 0; index < lastIndex; index++)
		{
		    verticesString += corners[index] + ", ";
		    if ((index + 1) % 10 == 0)
		        verticesString += "\n   ";
		}
		if (lastIndex >= 0)
		    verticesString += corners[lastIndex];
		verticesString += "}";

        // sträng med kanter
		String    edgesString = "{";
		int    counter = 0;
		for (int index = 0; index <= lastIndex; index++)
		{
			Node    node = routes[index];
			while (node != null)
			{
//                if (corners[index].compareTo (
//					corners[node.toIndex]) <= 0)
//                {
			        if (counter > 0  &&  counter % 5 == 0)
			            edgesString += "\n   ";
			        edgesString += "{" + corners[index] + ", "
			                    + corners[node.toIndex] + ", "
			                    + node.edgeWeight + "}, ";
			        counter++;
//				}

			    node = node.nextNode;
		    }
		}
		if (edgesString.length () > 1)
		    edgesString = edgesString.substring (0,
		                  edgesString.length () - 2);
		edgesString += "}";

        // sträng med både hörn och kanter
		String    string = "{ " + verticesString + ",\n  "
		                       + edgesString + " }";

		return string;
	}

}