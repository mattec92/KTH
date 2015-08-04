import java.io.*;
import java.util.*;

public class MorseTranslator {
	HashTable<String, String> table = new HashTable<String, String>(100);
	//Konstruktor, anropar loadTranslation som laddar in
	public MorseTranslator() throws IOException {
		loadTranslation();
	}
	//loadTranslation - Hämtar morse-alfabetet från en fil och lägger till i en hashtabell.
	public void loadTranslation() throws IOException {
		Scanner sc = new Scanner(new File("morse.txt"));
		//Så länge det finns mer i filen.
		while (sc.hasNext()) {
			String letter = sc.next();
			String morse = sc.next();
			//Lägg till en översättning i hashtabellen. Filen är formaterad så att på varje rad
			//står först bokstaven och sedan morseöversättningen. Lägg även till översättning åt andra hållet.
			table.put(letter, morse);
			table.put(morse, letter);
		}
//		System.out.println(table.toString());
	}
	//toMorse - returnerar en sträng översatt till morsekod.
	public String toMorse(String in) {
		//Sätter strängen till UPPERCASE
		in = in.toUpperCase();
		String out = "";
		//Går igenom alla tecken i strängen.
		for (int i = 0; i < in.length(); i++) {
			//Om tecknet är ett mellanslag, lägg till mellanslag.
			if (in.charAt(i) == ' ') {
				out += " ";
			}
			//Annars leta upp översättningen i hashtabellen med bokstaven som key.
			else {
				//Lägg till översättningen till strängen som ska returneras.
				out += table.get(Character.toString(in.charAt(i))) + " ";
			}
		}
		return out;
	}
	//toABC - returnerar en sträng översatt från morsekod till bokstäver, ABC.
	public String toABC(String in) {
		Scanner sc = new Scanner(in);
		sc.useDelimiter("[^-.]+");
		String out = "";
		//Så länge det finns fler morse-tecken i strängen
		while(sc.hasNext()) {
			//Lägg till översättningen till out.
			out += table.get(sc.next());
		}
		return out;
	}
	public static void main(String[] arg) throws IOException {
		MorseTranslator m = new MorseTranslator();
		System.out.println(m.toMorse("ABCDEFGHIJKLMNOPQSTUVWXYZ123456789"));
		System.out.println(m.toABC(".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----."));
		System.out.println(m.toMorse("hej tihi"));
		System.out.println(m.toABC(m.toMorse("hej tihi")));
	}
}