/**
 *Tarning.java
 *Laboration 4, 25 November
 *Mattias Cederlund
 *mcede@kth.se
 */

public class Tarning {
	protected final int sidor; //Final för att låsa värdet när tärningen väl skapats
    public Tarning(int sidor) {
    	this.sidor = sidor;
    }
    public int kasta() { //Metod för att kasta tärningen, returnerar ett tal mellan 1 och antal sidor tärningen har
    	return (int) (Math.random()*sidor) + 1;
    }
    public String toString() { //Metod för att "skriva ut" tärningen, beskriva tärningen i en sträng
    	return "Tärningen har " + sidor + " sidor.";
    }
}