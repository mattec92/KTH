import java.util.Scanner;

public class DanskSolver {
	public static void main(String[] args) {
    	Scanner sc = new Scanner(System.in);
    	while (true) {
    		String num = "";
			try {
				num = "" + dansk(sc.nextLine());
			} catch (NotDanishException e) {
				num = "Inte ett danskt räkneord mindre än 100!";
			}
    		System.out.println(num);
    	}
	}
	
	public static int dansk(String in) throws NotDanishException {
		String head = getHead(in);
		String rest = getRest(in);
		
		if (head.equals("nul") && rest.equals("")) {
			return 0;
		}
		else {
			return en2ni(in);
		}
	}
	
	public static int en2ni(String in) throws NotDanishException {
		String head = getHead(in);
		String rest = getRest(in);
		
		switch (head) {
		case "en":
			return 1 + mer(rest);
		case "et":
			return 1 + mer(rest);
		case "to":
			return 2 + mer(rest);
		case "tre":
			return 3 + mer(rest);
		case "fire":
			return 4 + mer(rest);
		case "fem":
			return 5 + mer(rest);
		case "seks":
			return 6 + mer(rest);
		case "syv":
			return 7 + mer(rest);
		case "otte":
			return 8 + mer(rest);
		case "ni":
			return 9 + mer(rest);
		default:
			return ti2nitten(in);
		}
	}

	private static int ti2nitten(String in) throws NotDanishException {
		String head = getHead(in);
		
		switch (head) {
		case "ti":
			return 10;
		case "ellve":
			return 11;
		case "tolv":
			return 12;
		case "tretten":
			return 13;
		case "fjorten":
			return 14;
		case "femten":
			return 15;
		case "seksten":
			return 16;
		case "sytten":
			return 17;
		case "atten":
			return 18;
		case "nitten":
			return 19;
		default:
			return tyve2halvfems(in);
		}
	}

	private static int tyve2halvfems(String in) throws NotDanishException {
		String head = getHead(in);
		String rest = getRest(in);
		
		switch (head) {
		case "tyve":
			return 20;
		case "tredive":
			return 30;
		case "fyrre":
			return 40;
		case "fyrretyve":
			return 40;
		default:
			return (int) (halvtreds2halvfems(in) * emfas(rest));
		}
	}

	private static double halvtreds2halvfems(String in) throws NotDanishException {
		String head = getHead(in);
		String rest = getRest(in);
		
		if (head.equals("halv")) {
			return (-0.5 + halvmult(rest));
		}
		else {
			return mult(in);
		}
	}

	private static int mult(String in) throws NotDanishException {
		String head = getHead(in);
		
		switch (head) {
		case "tres":
			return 3;
		case "firs":
			return 4;
		case "fjerds":
			return 4;
		default:
			throw new NotDanishException();
		}
	}

	private static int halvmult(String in) throws NotDanishException {
		String head = getHead(in);
		if (head.equals("fems")) {
			return 5;
		}
		else {
			return mult(in);
		}
	}
	
	private static double emfas(String in) throws NotDanishException {
		String head = getHead(in);
		String rest = getRest(in);
		
		if (head.equals("sindstyve") && rest.equals("")) {
			return 20;
		}
		else if (head.equals("sindstyve") && rest.equals("") == false) {
			throw new NotDanishException();
		}
		else {
			return 20;
		}
	}

	private static int mer(String in) throws NotDanishException {
		String head = getHead(in);
		String rest = getRest(in);
		
		if (head.equals("og")) {
			return tyve2halvfems(rest);
		}
		else  if (head.equals("")){ 
			return 0;
		}
		else {
			throw new NotDanishException();
		}
	}

	public static String getHead(String in) {
		Scanner sc = new Scanner(in);
		String out = "";
		if (sc.hasNext()) {
			out += sc.next();
		}
		sc.close();
		return out;
	}
	
	public static String getRest(String in) {
		Scanner sc = new Scanner(in);
		String out = "";
		if (sc.hasNext()) {
			sc.next();
		}
		while (sc.hasNext()) {
			out += sc.next() + " ";
		}
		sc.close();
		return out;
	}
}

class NotDanishException extends Exception
{
      //Parameterless Constructor
      public NotDanishException() {}

      //Constructor that accepts a message
      public NotDanishException(String message)
      {
         super(message);
      }
 }