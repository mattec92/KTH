//Skapar en osorterad lista och överför elementen till en sorterad lista.
public class UnsortedToSortedList {
	public static void main(String[] arg) {
		int[] values = {5,12,2,4,7,5,18,20,1,8};
		UnsortedList<Integer> unsorted = new UnsortedList<Integer>();
		//Lägger till specificerade element till den osorterade listan.
		for (int i = 0; i < values.length; i++) {
			unsorted.add(values[i]);
		}
		//Skapar en sorterad lista med den osorterade listan som argument.
		//Detta ger en sorterad lista med samma element som den osorterade.
		SortedList<Integer> sorted = new SortedList<Integer>(unsorted);
		System.out.println("Osorterad lista: " + unsorted);
		System.out.println("Sorterad lista: " + sorted);
	}
}