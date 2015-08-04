import java.util.*;
public class Factors {
	HashTable<Integer, int[]> table = new HashTable<Integer, int[]>(10000);
	//Konstruktor - kallar på funktionen som lägger till tal och faktorisering i hashtabellen.
	public Factors() {
		createTable();
	}
	//Lägger till alla tal mellan 0 och 10000 till tabellen, med deras faktoriseringar.
	public void createTable() {
		for (int i = 0; i < 10000; i++) {
			table.put(i, getFactors(i));
		}
	}
	//Metoden shorten - returnerar ett förkortat bråk
	public String shorten(int dividend, int divisor) {
		//Hämta täljare och nämnares faktoriseringar ur hashtabellen.
		int[] dividendFactors = table.get(dividend).clone();
		int[] divisorFactors = table.get(divisor).clone();
//		System.out.println(Arrays.toString(dividendFactors));
//		System.out.println(Arrays.toString(divisorFactors));
		//Gå igenom faktoriseringarna. Påträffas samma fxaktor i både täljaren och nämnaren "stryks" de, förkorats till en etta.
		for (int i = 0; i < dividendFactors.length; i++) {
			for (int j = 0; j < divisorFactors.length; j++) {
				//Om lika faktorer påträffas, ändra båda till 1 och avbryt inre loopen.
				if (dividendFactors[i] == divisorFactors[j]) {
					dividendFactors[i] = 1;
					divisorFactors[j] = 1;
					break;
				}
			}
		}
//		System.out.println(Arrays.toString(dividendFactors));
//		System.out.println(Arrays.toString(divisorFactors));
		int dividendTotal = 1;
		int divisorTotal = 1;
		//Multiplicera alla faktorer i täljaren.
		for (int i = 0; i < dividendFactors.length; i++) {
			dividendTotal = dividendTotal *  dividendFactors[i];
		}
		//Multiplicera alla faktorer i nämnaren.
		for (int i = 0; i < divisorFactors.length; i++) {
			divisorTotal = divisorTotal * divisorFactors[i];
		}
		//Returnera representation av bråket.
		return dividendTotal + " / " + divisorTotal;
	}
	//getPrimes - returnerar en array med alla primtal inom ett intervall.
	public int[] getPrimes(int start, int stop) {
		//Temporär array för att lagra primtalen, fel antal platser då det ännu är okänt.
		int[] temp = new int[stop-start];
		int count = 0;
		//Så länge man inte överstiger högsta värdet, sök upp talet i hashtabellen
		//och undersök om det är ett primtal, om arrayen som lagras bara har ett element.
		for (int i = start; i < stop; i++) {
			//Är det ett primtal, lägg till talet till den tempoärära arrayen.
			if (table.get(i).length == 1) {
				temp[count] = table.get(i)[0];
				count++;
			}
		}
		//När alla primtal hittats, skapa en array med rätt antal platser.
		int[] out = new int[count];
		//Överför värdena i den tempoärära arrayen till arrayen som ska returneras.
		for (int i = 0; i < count; i++) {
			out[i] = temp[i];
		}
		return out;
	}
	//getFactors - returnerar en int-array med alla faktorer till ett tal.
	public int[] getFactors(int in) {
		int[] factors = new int[100];
		int[] out;
		int divisions = 0;
		int i;
		//Så länge det minst går att rela in på 2.
		for (i = 2; i < in; i++) {
			//Om talet är delbart på något tal.
			if (in % i == 0) {
				//Lägg till det till arrayen och peka ut nästa plats.
				factors[divisions] = i;
				divisions++;
				//Dela begynnelsevärdet med faktorn.
				in = in / i;
				//Sätt i till 1 för att i nästa varv i loopen fortsätta på minsta möjliga faktor.
				i = 1;
			}
		}
		//Skapa array med rätt antal platser för att returnera.
		out = new int[divisions+1];
		//Om det gjorts divisioner, alltså om talet inte är ett primtal.
		if (divisions != 0) {
			//Lägg till sista faktorn
			factors[divisions] = i;
			//Skriv faktorerna från den temporära arrayen till den som ska returneras, med rätt antal platser.
			for (i = 0; i < factors.length && factors[i] != 0; i++) {
				out[i] = factors[i];
			}
		}
		//Om primtal, skriv till första platsen i arrayen som kommer ha 1 plats.
		else {
			out[0] = in;
		}
		return out;
	}
	public static void main(String[] arg) {
		Factors f = new Factors();
		System.out.println(Arrays.toString(f.getPrimes(100,200)));
		System.out.println(Arrays.toString(f.getPrimes(1009,1200)));
		System.out.println(f.shorten(12, 120));
		System.out.println(f.shorten(12, 60));
	}
}