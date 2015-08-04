public class User {
	int cardNr;
	int code;
	int balance;
	int nextCodeIndex = 0;
	int[] safetyCodes = new int[50];
	
	public User(int nr, int c, int b) {
		cardNr = nr;
		code = c;
		balance = b;
		for(int i = 0, j = 1; j < 100; i++, j = j + 2) {
			safetyCodes[i] = j;
		}
	}
	public int getCardNr() {
		return cardNr;
	}
	public boolean login(int c) {
		if (c == code) {
			return true;
		}
		return false;
	}
	public int getBalance() {
		return balance;
	}
	public void deposit(int amount) {
		balance += amount;
	}
	public boolean withdraw(int amount, int code) {
		if (code == safetyCodes[nextCodeIndex]) {
			balance -= amount;
			nextCodeIndex++;
			return true;
		}
		return false;
	}
}

