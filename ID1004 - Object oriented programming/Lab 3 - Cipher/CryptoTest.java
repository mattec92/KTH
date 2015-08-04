/**
 *CryptoTest.java
 *Laboration 3, 10 November
 *Mattias Cederlund
 *mcede@kth.se
 */

public class CryptoTest {
	public static void main(String[] arg) {
		//Skapar ett CesarCipher med nyckeln 2 och krypterar ett tecken, och dekrypterar ett tecken.
		CesarCipher c = new CesarCipher(2);
		System.out.println(c.encryptChar('A'));
		System.out.println(c.decryptChar('C'));
	}
}