/**
   @author Mattias Cederlund, mcede@kth.se
*/

//The user class.
public class User {
	private int cardNr;
	private int code;
	private int balance;
	private int nextCodeIndex = 0;
	private int[] securityCodes = new int[50];
	
	public User(int nr, int c, int b, int nc) {
		cardNr = nr;
		code = c;
		balance = b;
		nextCodeIndex = nc;
		for(int i = 0, j = 1; j < 100; i++, j = j + 2) {
			securityCodes[i] = j;
		}
	}
	//Returns the users card number.
	public int getCardNr() {
		return cardNr;
	}
	//Returns true if the provided PIN-code matches the users PIN-code.
	public boolean login(int c) {
		if (c == code) {
			return true;
		}
		return false;
	}
	//Returns the users balance.
	public int getBalance() {
		return balance;
	}
	//Deposits/adds the desired amount to the users balance.
	public void deposit(int amount) {
		balance += amount;
	}
	//Returns true if the provided security code matches the users next security code.
	//Subtracts desired amount from the users balance.
	public boolean withdraw(int amount, int code) {
		if (code == securityCodes[nextCodeIndex]) {
			balance -= amount;
			nextCodeIndex++;
			return true;
		}
		return false;
	}
	//Returns a String representation of the user.
	public String toString() {
		return "CardNR: " + cardNr + " PIN-code: " + code + " Balance: " + balance + 
				" Next security-code: " + securityCodes[nextCodeIndex];
	}
	//Returns a String for export of the users data.
	public String export() {
		return cardNr + " " + code + " " + balance + " " + nextCodeIndex;
	}
}

