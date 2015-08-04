public class Exchange {
	BinaryTreeMultiset<Integer> values;
	int total;
	//Konstruktor, skapar en "växelautomat"
	public Exchange(Set<Integer> set) {
		values = new BinaryTreeMultiset<Integer>(set);
	}
	//Returnerar en multimängd med minimalt antal valörer.
	public BinaryTreeMultiset<Integer> exchange() {
		//Adderar ihop alla valörer i given mängd och anropar giveChange-rutinen.
		for (int element : values) {
			total += element;
//			values.remove();
		}
		return giveChange(total);
	}
	//giveChange - returnerar en multimängd med minimalt antal valörer.
	public BinaryTreeMultiset<Integer> giveChange(int in) {
		BinaryTreeMultiset<Integer> result = new BinaryTreeMultiset<Integer>();
		//Array med alla möjliga valörer i sjunkande ordning.
		int[] values = {1000, 500, 100, 50, 20, 10, 5, 1};
		//Körs så länge man inte gått igenom alla typer av valörer, och det finns saldo på "kontot"
		//som ska växlas ut.
		for (int i = 0; i < values.length && in > 0; i++) {
			//Så länge man kan ge fler sedlar/mynt av samma valör. När det inte längre går
			//Byts valören ut mot en lägre och man försöker igen.
			while (in >= values[i]) {
				//Dra bort valören från saldot och lägg till valören i multimängden.
				in = in - values[i];
				result.add(values[i]);
			}
		}
		return result;
	}
	public static void main(String[] arg) {
		//Skapa två arrayer med valörer
		int[] values1 = {5,10,20,50,1,100,500,5,5,20};
		int[] values2 = {5,100,20,100,500,1000,20,1,1,1,5};
		//Skapa två multimängder och lägg valörerna i mängderna
		BinaryTreeMultiset<Integer> kassa1 = new BinaryTreeMultiset<Integer>();
		BinaryTreeMultiset<Integer> kassa2 = new BinaryTreeMultiset<Integer>();
		for(int i : values1) {
			kassa1.add(i);
		}
		for(int i : values2) {
			kassa2.add(i);
		}
		System.out.println("Ursprungliga kassa1: " + kassa1);
		System.out.println("Ursprungliga kassa2: " + kassa2);
		//Slå ihop kassorna och cleara den kassan som nu är tom.
		kassa1 = kassa1.union(kassa2);
		kassa2.clear();
		System.out.println("Kassan efter ihopslagningen: " + kassa1);
		//Skapa en ny bank och lägg in kassan i banken.
		Exchange bank = new Exchange(kassa1);
		//Anropa exhange-rutinen, som ger dig minsta möjliga multimängd valörer tillbaka.
		kassa1 = bank.exchange();
		System.out.println("Kassan efter växlingen: " + kassa1);
	}
}