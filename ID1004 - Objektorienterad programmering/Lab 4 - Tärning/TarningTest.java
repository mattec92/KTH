/**
 *TarningTest.java
 *Laboration 4, 25 November
 *Mattias Cederlund
 *mcede@kth.se
 */

//Testprogram för tärningarna
public class TarningTest {
	public static void main(String[] arg) {
		Tarning[] tarningar = {new T4(), new T8(), new T20()}; //Skapar tre tärningar
		for (int j = 0; j < 3; j++) {
			for (int i = 0; i < 3; i++) {
				//Kastar varje tärning tre gånger, i ordningen de ligger i arrayen tarningar. Visar även vilken tärning som kastats.
				System.out.println(tarningar[j].toString() + " Kastet blev " + tarningar[j].kasta());
			}
		}
	}
}