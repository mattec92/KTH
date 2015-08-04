import javax.swing.*;
import java.awt.*;
import java.io.*;

public class TowersOfHanoi extends JFrame{
	//Skapa tre köer som symboliserar tornen.
	static minOnlyLifoQueue<Integer> pole1 = new minOnlyLifoQueue<Integer>();
	static minOnlyLifoQueue<Integer> pole2 = new minOnlyLifoQueue<Integer>();
	static minOnlyLifoQueue<Integer> pole3 = new minOnlyLifoQueue<Integer>();
	int diskCount;

	public TowersOfHanoi(int disks) {
		diskCount = disks;
		//Lägg till diskar på start-tornet.
		for (int i = diskCount; i > 0; i--) {
			pole1.put(i);
			System.out.println(pole1.peek());
		}
		//Inställningar för rutan grafiken visas i.
		this.setVisible(true);
		this.setSize(1150, 300);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setResizable(true);
		//Gör förflyttning från torn1, till torn3, via torn 2.
		move(disks, 1, 3, 2);
	}
	//Move-metoden. Flyttar en disk.
	public void move(int n, int from, int to, int via) {
		int element;

 		if (n == 1) {
 			try {
			//Skapa fördröjning mellan varje förflyttning.
 			Thread.sleep(500);
	 		}
	 		catch (Exception e) {
	 		}
	 		//Rita om grafiken med den nya placeringen av diskarna.
	 		repaint();
 			//Bestämmer vilken kö/torn diskarna ska placeras i. Placerar disken på tornet.
 			if (from == 1) {
 				element = pole1.take();
 			}
 			else if (from == 2) {
 				element = pole2.take();
 			}
 			else {
 				element = pole3.take();
 			}
 			if (to == 1) {
 				pole1.put(element);
 			}
 			else if (to == 2) {
 				pole2.put(element);
 			}
 			else {
 				pole3.put(element);
 			}
   			System.out.println("Move disk sized " + element + " from pole " + from + " to pole " + to);
  		}
  		//Rekursiva lösningsmetoden för Towers of Hanoi-problemet.
  		else {
    		move(n - 1, from, via, to);
    		move(1, from, to, via);
    		move(n - 1, via, to, from);
  		}
	}
	//Metoden paint, ritar ut grafisk representation över tornen och diskarna.
	public void paint(Graphics g) {
		lifoQueue<Integer> temp = new lifoQueue<Integer>();
		g.setColor(Color.WHITE);
		g.fillRect(0 , 0, 1200, 1000);
		g.setColor(Color.BLACK);
		//Ritar tornen på rätt plats beroende av antalet diskar
		g.fillRect(90+(diskCount)*20, 50, 20, 200);
		g.fillRect(390+(diskCount)*20, 50, 20, 200);
		g.fillRect(690+(diskCount)*20, 50, 20, 200);
		g.fillRect(0, 250, 1200 , 10);
		g.setColor(Color.RED);
		//Temporär size-variabel. Eftersom vi tar ut objekt ur kön under utritning kommer inte listans storlek vara konstant.
		int size = pole1.size();
		//Ritar diskarna på det första tornet på rätt plats.
		//Positionen bestäms av x = (grundposition - översta diskens storlek * grundstorlek),
		//y = (grundposition - (antal diskar - nuvarande översta disk) * grundstorlek + (antal diskar - antal diskar på tornet) * grundstorlek
		//Storleken bestäms av diskens storlek som lagras i kön, gånger en grundstorlek.
		for (int i = 0; i < size; i++) {
			//Hämtar översta disken från tower1's kö och lägger den i en temporär lifokö. Ritar ut disken.
			temp.put(pole1.take());
			g.fillRect(100+(diskCount-temp.peek())*20, 250-(diskCount-i)*20+(diskCount-size)*20, temp.peek()*40, 20);
		}
		//Lägger tillbaka diskarna på tornet i motsatt ordning man tog ut dem. Alltså hamnar alla diskar på samma plats igen.
		for (int i = 0; i < size; i++) {
			pole1.put(temp.take());
		}
		size = pole2.size();
		//Samma som för första tornet.
		for (int i = 0; i < size; i++) {
			temp.put(pole2.take());
			g.fillRect(400+(diskCount-temp.peek())*20, 250-(diskCount-i)*20+(diskCount-size)*20, temp.peek()*40, 20);
		}
		for (int i = 0; i < size; i++) {
			pole2.put(temp.take());
		}
		size = pole3.size();
		//Samma som för första tornet.
		for (int i = 0; i < size; i++) {
			temp.put(pole3.take());
			g.fillRect(700+(diskCount-temp.peek())*20, 250-(diskCount-i)*20+(diskCount-size)*20, temp.peek()*40, 20);
		}
		for (int i = 0; i < size; i++) {
			pole3.put(temp.take());
		}
	}

	public static void main(String[] args) {
		//Skapa ny instans av Towers of Hanoi-problemet.
		TowersOfHanoi towers = new TowersOfHanoi(3);
		System.out.println(towers.pole1.size());
		System.out.println(towers.pole2.size());
		System.out.println(towers.pole3.size());
	}
}
