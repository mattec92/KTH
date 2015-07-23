public class ArithmeticSolver {
	flQueue<Character> operatorStack = new flQueue<Character>();
	flQueue<Double> operandStack = new flQueue<Double>();
	String sentence;

	//Konstruktor, sätter argumentet till en klassvariabel för synlighet för resten av klassen.
	//Samt anropar solvning-metoden och skriver ut resultatet.
	public ArithmeticSolver(String in) {
		sentence = in;
		System.out.println("Svar:" + solve());
	}
	//Metoden solve, beräknar ett aritmetiskt uttryck.
	public double solve() {
		int index = 0;
		double lastValue = 0;
		//Så länge inte hela uttrycket gåtts igenom.
		while (index < sentence.length()) {
			//Sätt c till att vara tecknet på aktuell position
			char c = sentence.charAt(index);
			//Om tecknet är en siffra
			if (Character.isDigit(c) == true) {
				double value = 0;
				int i;
				//Kolla om efterföljande tecken är siffor
				for (i = index+1 ; i < sentence.length(); i++) {
					if (!Character.isDigit(sentence.charAt(i))) {
						break;
					}
				}
				//När första tecknet som inte är en siffra påträffas lägg till talet till operandstacken.
				value = Double.parseDouble(sentence.substring(index, i));
				System.out.println("Digit:" + value + " Index:" + index);
				//Öka indexpekaren till att peka på nästa tecken som inte ingick i talet.
				index = i;
				//Lägg operanden på stacken.
				operandStack.putLast(value);
			}
			//Om ett + eller, lägg det på operatorstacken, öka indexpekaren.
			else if (c == '+') {
				System.out.println("+:" + c + " Index:" + index);
				operatorStack.putLast(c);
				index++;
			}
			//Om ett - påträffas, lägg det efterföljande negtiva operanden på operandstacken, samt ett + på operatorstacken.
			else if (c == '-') {
				double value = 0;
				boolean doubleminus = false;
				int i;
				//Kolla om efterföljande tecken är siffor.
				for (i = index+1 ; i < sentence.length(); i++) {
					//Om nästföljande tecken också är ett minustecken, hoppa fram startindexet ett steg för att undvika att båda
					//minustecknen tas med när man försöker hitta operanden. (2 minustecken tar ut varandra och kan lika gärna hoppas över.)
					if (sentence.charAt(i) == '-') {
						//Index sätts till += 2 för att "hoppa över" båda minustecknen.
						index += 2;
						//doubleminus-flaggan sätts till true vilket signalerar senare att man inte ska försöka lägga till varken operand eller operator
						doubleminus = true;
						break;
					}
					else if (!Character.isDigit(sentence.charAt(i))) {
						break;
					}
				}
				//Operator och operand ska endast läggas till köerna ifall att man inte påträffat dubbla minustecken.
				if (doubleminus == false) {
					//När första tecknet som inte är en siffra påträffas lägg till talet till operandstacken.
					value = Double.parseDouble(sentence.substring(index, i));
					System.out.println("Digit:" + value + " Index:" + index);
					//Öka indexpekaren till att peka på nästa tecken som inte ingick i talet.
					index = i;
					//Lägg operanden på stacken.
					operandStack.putLast(value);
					//Lägg till ett + på operatorstacken. Att addera ett negativt tal blir samma sak som att subtrahera samma positiva tal.
				}
				operatorStack.putLast('+');
			}
			//Om et * eller / påträffas, lägg det på operatorstacken.
			else if (c == '*' || c == '/') {
				System.out.println("*//:" + c + " Index:" + index);
				operatorStack.putLast(c);
				double value = 0;
				int i = index+1;
				//Om efterföljande tecken är ett minustecken, gå fram en position för att hitta nästa operand
				if (sentence.charAt(i) == '-') {
					i++;
				}
				//Leta sedan fram efterföljande operand
				for (; i < sentence.length(); i++) {
					if (!Character.isDigit(sentence.charAt(i))) {
						break;
					}
				}
				value += Double.parseDouble(sentence.substring(index+1, i));
				System.out.println("Digit:" + value + " Index:" + (index+1));
				index = i;
				//Utför operation mellan tidigare funnen operand och nyligen funnen operand.
				operandStack.putLast(arithmeticOperation(operatorStack.takeLast(), (double) operandStack.takeLast(), value));
			}
			//Om något ogiltigt tecken påträffas, avsluta.
			else {
				System.out.println("Felaktiga tecken!");
				System.exit(0);
			}
		}
		//När hela serien gåtts igenom och alla multiplikationer och divisioner utförs, addera och subtrahera återstående tal.
		//Om det finns fler eller lika många operatorer som operander, så är den första operatorn ett tecken till första operanden.
//		if (operatorStack.size() >= operandStack.size()) {
//			System.out.println("Lika många eller fler operatorer än operander");
//			lastValue = arithmeticOperation(operatorStack.takeFirst(), 0, operandStack.takeFirst());
//		}
//		else {
			lastValue = operandStack.takeFirst();
//		}
		//lastValue - Senaste framräknade värdet sätts till det första värdet i kön.
		//Så länge det finns fler operatorer, utför operationer på operanderna från vänster med lastValue och nästa operand.
		while (operatorStack.size() > 0 && operandStack.size() > 0) {
			lastValue = (arithmeticOperation(operatorStack.takeFirst(), lastValue, operandStack.takeFirst()));
		}
		return lastValue;
	}
	//Utför aritmetik operation, +-*/ mellan två tal beroende av vilken operand som skickas som argument.
	public double arithmeticOperation(char operator, double firstOperand, double secondOperand) {
		if (operator == '+') {
			return firstOperand + secondOperand;
		}
		else if (operator == '-') {
			return firstOperand - secondOperand;
		}
		else if (operator == '*') {
			return firstOperand * secondOperand;
		}
		else if (operator == '/') {
			return firstOperand / secondOperand;
		}
		return Integer.MAX_VALUE;
	}
	public static void main(String [] arg) {
		//Skapa ny solver med argumentet: vad som ska beräknas.
		ArithmeticSolver solver = new ArithmeticSolver("-12*-8+1/5--2*3*4");
	}
}