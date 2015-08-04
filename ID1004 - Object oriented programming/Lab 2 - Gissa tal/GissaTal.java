/*GissaTal.java
 *Laboration 2, 3 November 2011
 *Mattias Cederlund
 *mcede@kth.se
 */

import java.util.*;

public class GissaTal {
	public static void main(String[] arg) {
		Scanner sc = new Scanner(System.in);
		int tal = (int) (Math.random() *  101); //Det rätta talet, slumpas mellan 0 och 100
		int gissning = 0;
		LinkedList<Integer> g = new LinkedList<Integer>();
		int i;
		System.out.println("Gissa ett tal mellan 0-100");
		//Spelet körs så länge man inte gissat rätt och max 10 gånger
		for (i = 0; tal != gissning && i < 10; i++) {
			//Läs in nästa tal från tangentbordet
			gissning = sc.nextInt();
			//Spara gissningen till listan g
			g.add(gissning);
			//Spelaren gissar rätt
			if (gissning == tal) {
				System.out.println("Du gissade rätt! Det rätta talet var: " + tal);
			}
			//Spelaren gissar för lågt
			else if (gissning < tal) {
				System.out.println("Du gissade för lågt. Talet är högre än: " + gissning + "\nDu har " + (9-i) + " gissningar kvar.");
			}
			//Spelaren gissar för högt
			else if (gissning > tal) {
				System.out.println("Du gissade för högt. Talet är lägre än: " + gissning + "\nDu har " + (9-i) + " gissningar kvar.");
			}
		}
		//Om spelaren gissat 10 gånger utan att gissa rätt, ge det rätta talet
		if (i >= 9) {
			System.out.println("Du har gjort 10 gissningar utan att gissa rätt. Det rätta talet är: " + tal);
		}
		//Skriva ut spelarens gissningar i samma följd som de gjordes
		System.out.print("Dina gissningar: ");
		for (int j = 0; j < g.size()-1; j++) {
			System.out.print(g.get(j) + ", ");
		}
		System.out.print(g.get(g.size()-1)); //Sista gissningen utanför for-satsen för att slippa ett "," i slutet ;)
	}
}