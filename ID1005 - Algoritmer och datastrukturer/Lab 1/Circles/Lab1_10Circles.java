import java.util.*;
import java.awt.*;
import javax.swing.*;

public class Lab1_10Circles extends JFrame {
	private static Circle[] circles;
	private static Circle[] start;

	public static void main (String[] arg) {
		new Lab1_10Circles(15);
	}
	//Konstruktor, skapar cirklar, sorterar
	public Lab1_10Circles(int num) {
		circles = new Circle[num];
		start = new Circle[num];
		Random rnd = new Random();
		//Skapa cirklar och spara en kopia av den ursprungliga ordningen
		for (int i = 0; i < num; i++) {
			circles[i] = new Circle(rnd.nextInt(10)+1, rnd.nextInt(3));
			start[i] = circles[i];
		}
		//Sortera cirklarna och visa grafisk representation
		Arrays.sort(circles);
		this.setVisible(true);
		this.setSize(86*num, 300);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	public void paint(Graphics g) {
		g.clearRect(0, 0, 86*circles.length, 300);
		//Ritar ut grafisk representation av de osorterade cirklarna
		for (int i = 0; i < Lab1_10Circles.start.length; i++) {
			int xoffset = 77+77*i;
			int radie = Lab1_10Circles.start[i].radie*7;
			String st = "" + Lab1_10Circles.start[i].radie;
			g.setColor(Color.BLACK);
			g.drawString(st, xoffset, 50);
			//Om cirkeln är gul, rita en gul cirkel med motsvarande area
			if (Lab1_10Circles.start[i].color == 0) {
				g.setColor(Color.YELLOW);
				g.fillOval(xoffset, 50, radie, radie);
			}
			//Om cirkeln är blå, rita en gul cirkel med motsvarande area
			else if (Lab1_10Circles.start[i].color == 1) {
				g.setColor(Color.BLUE);
				g.fillOval(xoffset, 50, radie, radie);
			}
			//Om cirkeln är röd, rita en gul cirkel med motsvarande area
			else {
				g.setColor(Color.RED);
				g.fillOval(xoffset, 50, radie, radie);
			}
		}
		//Ritar ut grafisk representation av de sorterade cirklarna
		for (int i = 0; i < Lab1_10Circles.circles.length; i++) {
			int xoffset = 77+77*i;
			int radie = Lab1_10Circles.circles[i].radie*7;
			String st = "" + Lab1_10Circles.circles[i].radie;
			g.setColor(Color.BLACK);
			g.drawString(st, xoffset, 200);
			//Om cirkeln är gul, rita en gul cirkel med motsvarande area
			if (Lab1_10Circles.circles[i].color == 0) {
				g.setColor(Color.YELLOW);
				g.fillOval(xoffset, 200, radie, radie);
			}
			//Om cirkeln är blå, rita en gul cirkel med motsvarande area
			else if (Lab1_10Circles.circles[i].color == 1) {
				g.setColor(Color.BLUE);
				g.fillOval(xoffset, 200, radie, radie);
			}
			//Om cirkeln är röd, rita en gul cirkel med motsvarande area
			else {
				g.setColor(Color.RED);
				g.fillOval(xoffset, 200, radie, radie);
			}
		}
	}
}
//Klassen cirkel
class Circle implements Comparable {
	final int YELLOW = 0;
	final int BLUE = 1;
	final int RED = 2;
	int radie;
	int color;

	public Circle(int rad, int col) {
		radie = rad;
		color = col;
	}
	//Metoden compareTo - Specificerar hur cirklar ska jämföras med varandra, vilken som är "störst" osv
	public int compareTo(Object cir) {
		Circle circle = (Circle) cir;
		//Om cirkeln i argumentet har störst radie
		if (circle.radie < this.radie) {
			return 1;
		}
		//Om båda cirklarna har lika radie, bestäm vilken som är "störst" beroende på färgen
		else if (circle.radie == this.radie) {
			//Om cirkeln i argumentet har har lägre prioriterad färg är den störst
			if (circle.color < this.color) {
				return 1;
			}
			//Om båda cirklarna har samma färg är båda cirklarna lika stora
			else if (circle.color == this.color) {
				return 0;
			}
			//Om cirkeln i argumentet har högre prioriterad färg är den minst (lågt = hög prio)
			else {
				return -1;
			}

		}
		//Om cirkeln i argumentet har minst radie
		else {
			return -1;
		}
	}
	//Metoden toString() - Stringrepresentation av cirklarna (Användes vid debug innan grafiskt kom till)
	public String toString() {
		if (this.color == YELLOW) {
			return (this.radie + ":Yellow");
		}
		else if (this.color == BLUE) {
			return (this.radie + ":Blue");
		}
		else {
			return (this.radie + ":Red");
		}
	}
}