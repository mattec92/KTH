/*
 *ID1004 Inlämningsuppgift 4.5hp
 *Memory
 *Mattias Cederlund
 *mcede@kth.se
 */

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.util.*;
import java.awt.event.*;

//Klassen Memory - Spelklassen
public class Memory extends JFrame implements ActionListener {
	private JPanel panel = new JPanel(new GridLayout(9, 8));
	protected static LinkedList<Card> cards = new LinkedList<Card>();
	protected static LinkedList<Player> players = new LinkedList<Player>();
	private LinkedList<ImageIcon> bilder = new LinkedList<ImageIcon>();
	private LinkedList<JLabel> scorelabels = new LinkedList<JLabel>();
	private LinkedList<JLabel> playerlabels = new LinkedList<JLabel>();
	private javax.swing.Timer timer = new javax.swing.Timer(3000, this);
	private int saveid = -1, savepos = -1, cardsturned = 0;
	private ImageIcon bk = new ImageIcon("Memorypics/baksida.jpg");
	static AI ai = new AI("hej",1);
	private AIHandler AIh = new AIHandler();
	public Memory(LinkedList<Player> players) {
		super("Memory -");
		this.players = players;
		makeBoard();
		this.add(panel);
		this.setVisible(true);
		this.setSize(640,720);
		this.setResizable(false);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		//Ger den första spelaren turen
		players.get(0).myTurn = true;
		playerlabels.get(0).setForeground(Color.GREEN);
		//Startar endast tråden för AI-hantering om det finns något AI med i spelet
		for (int i = 0; i < players.size(); i++) {
			if (players.get(i).getClass() == ai.getClass()) {
				AIh.start();
				break;
			}
		}
	}
	//Metoden makeBoard - Skapar de grafiska komponenterna för spelplanen
	public void makeBoard() {
		int k = 0;
		//Laddar in bilderna för kortens baksida till programmet
		for (int j = 1; j < 33; j++) {
			for (int i = 0; i < 2; i++) {
				bilder.add(new ImageIcon("Memorypics/" + j + ".gif"));
			}
		}
		//Skapar korten med bilder och id, blandar sedan korten
		for (int i = 0; i < 64; i++) {
			cards.add(new Card(bilder.get(i), k));
			if (i % 2 == 1) {
				k++;
			}
			Collections.shuffle(cards);
		}
		//Lägger ut korten på spelplanen
		for (int i = 0; i < 64; i++) {
			panel.add(cards.get(i));
			cards.get(i).addActionListener(this);
			cards.get(i).setBorder(new LineBorder(Color.WHITE, 1));
			cards.get(i).pos = i;
		}
		//Lägger till labels för nick och poängställning
		for (int i = 0; i < players.size(); i++) {
			playerlabels.add(new JLabel(players.get(i).nick + ": "));
			scorelabels.add(new JLabel(players.get(i).score + ""));
			panel.add(playerlabels.get(i));
			panel.add(scorelabels.get(i));
		}
	}
	//Metoden updateLabels - Uppdaterar labels med vems tur det är samt poängställningen
	public void updateLabels(boolean in) {
		for (int i = 0; i < players.size(); i++) {
			//Om spelaren vars tur det är väljer rätt kort ges en poäng, och spelaren får försöka en gång till
			if (players.get(i).myTurn && in) {
				players.get(i).score++;
				scorelabels.get(i).setText(players.get(i).score + "");
				//Om det finns kort kvar,  starta AIHandlern för att eventuellt AI ska göra sitt drag
				if (isCards()) {
					AIh.interrupt();
				}
				else {
					this.dispose();
					new Scorelist(players);
				}
				break;
			}
			//Om spelaren vars tur det är väljer fel kort blir det nästa spelares tur
			else if (players.get(i).myTurn && !in) {
				players.get(i).myTurn = false;
				players.get((i+1) % players.size()).myTurn = true;
				playerlabels.get(i).setForeground(Color.BLACK);
				playerlabels.get((i+1) % players.size()).setForeground(Color.GREEN);
				//Starta AIHandlern
				AIh.interrupt();
				break;
			}
		}
	}
	//Metoden isCards - Kollar om det finns kort kvar, genom att kolla spelarnas sammanlagda poäng
	public boolean isCards() {
		int sum = 0;
		for (int i = 0; i < players.size(); i++) {
			sum += players.get(i).score;
		}
		//Om den sammanlagda summan av poäng spelarna har är mindre än 32, finns det kort kvar
		if (sum < 32) {
			return true;
		}
		else {
			return false;
		}
	}
	//Actionevent - vad som händer om man klickar på ett kort
	public void actionPerformed(ActionEvent e) {
		for (int i = 0; i < cards.size(); i++) {
			if (e.getSource() == cards.get(i)) {
				//Lägger till de vända korten i minnet
				for (int k = 0; k < players.size(); k++) {
					//Lägger endast till kort om spelaren är ett AI
					if (players.get(k).getClass() == ai.getClass()) {
						//Kort ska inte läggas till minnet om det redan finns i minnet
						if (players.get(k).cardmemory.indexOf((Card) e.getSource()) == -1) {
							players.get(k).cardmemory.add((Card) e.getSource());
						}
						//Om det finns fler kort i minnet än minnets storlek, ta bort det första
						while (players.get(k).cardmemory.size() > players.get(k).mem) {
							players.get(k).cardmemory.removeFirst();
						}
					}
				}
				//Om det är det första kortet spelaren väljer, vänd kortet
				if (cardsturned == 0) {
					cards.get(i).setIcon(cards.get(i).back);
					cards.get(i).setBorder(new LineBorder(Color.RED, 3));
					saveid = cards.get(i).id;
					savepos = i;
					cardsturned = 1;
				}
				//Om det är det andra kortet spelaren väljer, och det är likadant som det första
				else if (saveid == cards.get(i).id && savepos != i && cardsturned == 1) {
					cards.get(i).setIcon(cards.get(i).back);
					cards.get(i).setBorder(new LineBorder(Color.GREEN, 3));
					cards.get(savepos).setBorder(new LineBorder(Color.GREEN, 3));
					cardsturned = 3;
					cards.get(i).taken = true;
					cards.get(savepos).taken = true;
					//Startar timer för att vända tillbaka eller ta bort kort, samt uppdatera labels
					timer.start();
					//Om en spelare har gissat rätt tar AIn bort korten från minnet, genom att ta bort alla tagna kort ur minnet
					for (int k = 0; k < cards.size(); k++) {
						if (cards.get(k).taken == true) {
							for (int j = 0; j < players.size(); j++) {
								players.get(j).cardmemory.remove(cards.get(k));
							}
						}
					}
				}
				//Om det är det andra kortet spelaren väljer, och det inte är likadant som det första
				else if (saveid != cards.get(i).id && savepos != i && cardsturned == 1) {
					cards.get(i).setBorder(new LineBorder(Color.RED, 3));
					cards.get(i).setIcon(cards.get(i).back);
					cardsturned = 2;
					//Startar timer för att vända tillbaka eller ta bort kort, samt uppdatera labels
					timer.start();
				}
				//Om spelaren väljer samma kort igen händer ingenting (Ursprungligen en utskrift i konsollen här)
				//Jag har helt enkelt valt att spelet inte ens ska reagera om man väljer samma kort igen (Signalerar dock att det inte går att välja)
				else if (savepos == i) {
				}
			}
			//När två kort är valda som inte är likadana startas en timer som bestämmer när korten ska vändas tillbaka
			if (e.getSource() == timer) {
				timer.stop();
				//Om spelaren gissat fel vänds korten tillbaka, labels uppdateras med indatan true
				if (cardsturned == 2) {
					updateLabels(false);
				}
				//Om spelaren gissat rätt tas korten bort, labels uppdateras med indatan true
				else if (cardsturned == 3) {
					updateLabels(true);
				}
				turnCards();
				cardsturned = 0;
			}
		}
	}
	//Metoden turnCards - Plockar bort alla kort som är tagna och vänder tillbaka alla kort som inte är tagna
	public void turnCards(){
		for (int i = 0; i < cards.size(); i++) {
			cards.get(i).setIcon(bk);
			cards.get(i).setBorder(new LineBorder(Color.WHITE, 1));
			if (cards.get(i).taken == true) {
				cards.get(i).setVisible(false);
			}
		}
	}
	//MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN MAIN
	public static void main(String[] arg) {
		new Menu();
	}
}
//Klassen Card - kortobjekten som ligger på spelplanen, ärvs från JButton
class Card extends JButton {
	final protected int id;
	protected int pos;
	protected boolean taken = false;
	protected ImageIcon back;
	public Card(ImageIcon img, int id) {
		super(new ImageIcon("Memorypics/baksida.jpg"));
		this.id = id;
		back = img;
	}
}
//Klassen Player -  spelarobjektet
class Player {
	protected int score, mem;
	final protected String nick;
	protected boolean myTurn;
	protected LinkedList<Card> cardmemory = new LinkedList<Card>();
	public Player(String nick) {
		this.nick = nick;
	}
	public void play() {
	}
}
//Klassen AI - datorstyrd spelare, ärver från Player
class AI extends Player {
	public AI(String nick, int mem) {
		super(nick);
		this.mem = mem;
	}
	//Metoden play - Strategin AIt använder för att göra sitt drag
	public void play() {
		boolean twoInMem = false;
		boolean guessSameAsMem = false;
		int firstguess, secondguess;
		//Kollar om den känner till två likadana kort
		for (int i = 0; i < cardmemory.size(); i++) {
			for (int j = i+1; j < cardmemory.size(); j++) {
				if (cardmemory.get(i).id == cardmemory.get(j).id && cardmemory.get(i).pos != cardmemory.get(j).pos) {
					Memory.cards.get(cardmemory.get(i).pos).doClick();
					Memory.cards.get(cardmemory.get(j).pos).doClick();
					twoInMem = true;
					break;
				}
			}
			if (twoInMem == true) {
				break;
			}
		}
		//Om den inte har två likadana kort - gissa ett kort
		if (twoInMem == false) {
			firstguess = guess();
			Memory.cards.get(firstguess).doClick();
			//Kollar om ett likadant kort som det framgissade finns i minnet, om sant, välj kortet
			for (int i = 0; i < cardmemory.size(); i++) {
				if (cardmemory.get(i).id == Memory.cards.get(firstguess).id && cardmemory.get(i).pos != Memory.cards.get(firstguess).pos) {
					Memory.cards.get(cardmemory.get(i).pos).doClick();
					twoInMem = true;
					break;
				}
			}
			//Om ett likadant kort som gissningen inte finns i minnet, gissa ett till kort
			if (twoInMem == false) {
				//Gissningarna får inte vara på samma kort, kommer köras tills två olika kort valts
				while (true) {
					secondguess = guess();
					if (firstguess != secondguess) {
						break;
					}
				}
				Memory.cards.get(secondguess).doClick();
			}
		}
		twoInMem = false;
		guessSameAsMem = false;
	}
	//Metoden guess - Gissar fram ett kort
	public int guess() {
		int guess;
		boolean guessSameAsMem = false;
		while (true) {
			//Gör en gissning
			guess = (int) (Math.random() * 64);
			//Kolla så att kortet man gissat inte finns i minnet
			for (int i = 0; i < cardmemory.size(); i++) {
				if (guess == cardmemory.get(i).pos) {
					guessSameAsMem = true;
					break;
				}
			}
			//Om det framgissade kortet inte finns i minnet och inte redan är taget, välj kortet
			if (Memory.cards.get(guess).taken == false && guessSameAsMem == false) {
				return guess;
			}
			//Annars, nollställ värden och gör en ny gissning
			guessSameAsMem = false;
		}
	}
}
//Klassen Menu - skapar en meny med spelinställningar
class Menu extends JFrame implements ActionListener {
	private JPanel panel;
	private String[] labeltext = {"Spelare", "Nick", "AI", "Intellegens", "Spelare 1", "Spelare 2", "Spelare 3", "Spelare 4"};
	private LinkedList<JTextField> textfields = new LinkedList<JTextField>();
	private LinkedList<JTextField> intfields = new LinkedList<JTextField>();
	private LinkedList<JCheckBox> boxes = new LinkedList<JCheckBox>();
	private LinkedList<Player> players = new LinkedList<Player>();
	private JButton go = new JButton("Starta");
	private JButton help = new JButton("Hjälp");
	private JFrame f = new JFrame("Hjälp");
	public Menu() {
		super("Meny");
		panel = new JPanel(new GridLayout(7,4));
		createLabels();
		this.add(panel);
		this.setVisible(true);
		this.setSize(300,200);
		this.setResizable(false);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	//Skapar de grafiska komponenterna för menyn
	public void createLabels() {
		//Skapar textfält för nick och intellegens och lägger till dem i en lista
		for (int i = 0; i < 4; i++) {
			textfields.add(new JTextField(20));
			intfields.add(new JTextField(20));
		}
		//Skapar checkboxes för att bestämma om spelaren är ett AI och lägger till den i en lista
		for (int i = 0; i < 4; i++) {
			boxes.add(new JCheckBox());
			boxes.get(i).addActionListener(this);
		}
		//Lägger till labels med förbestämd text i panelen
		for (int i = 0; i < 4; i++) {
			panel.add(new JLabel(labeltext[i]));
		}
		//Lägger till textfields och checkboxes i panelen
		for (int i = 4; i < 8; i++) {
			panel.add(new JLabel(labeltext[i]));
			panel.add(textfields.get(i-4));
			panel.add(boxes.get(i-4));
			panel.add(intfields.get(i-4));
			intfields.get(i-4).setVisible(false);
		}
		//Lägger till några tomma labels i panelen för att skapa mellanrum och symetri
		for (int i = 0; i < 5; i++) {
			panel.add(new JLabel(" "));
		}
		//Lägger till knapparna och lägger actionlisteners på dem
		panel.add(go);
		go.addActionListener(this);
		panel.add(help);
		help.addActionListener(this);
	}
	//Action - vad som händer om man klickar på de olika knapparna
	public void actionPerformed(ActionEvent e) {
		//Om man trycker på startknappen
		if (e.getSource() == go) {
			for (int i = 0; i < 4; i++) {
				//Skapar spelare och AI, beroende på om textfälten och checkboxarna är ifyllda
				if (textfields.get(i).getText().length() > 0) {
					//Om checkboxen är itryckt, skapa ett AI
					if (boxes.get(i).isSelected()) {
						try {
							if (Integer.parseInt(intfields.get(i).getText()) >= 0) {
								players.add(new AI(textfields.get(i).getText(), Integer.parseInt(intfields.get(i).getText())));
							}
							//Om intellegensen är angivet negativt använder vi ett minne av storleken 0.
							else {
								players.add(new AI(textfields.get(i).getText(), 0));
							}
						}
						//Om intellegensen inte är angiven i siffror, använd standardintellegensen på 20 korts minne
						catch (NumberFormatException ex) {
							players.add(new AI(textfields.get(i).getText(), 20));
						}
					}
					//Om spelaren inte är ett AI, lägg till en vanlig spelare
					else {
						players.add(new Player(textfields.get(i).getText()));
					}
				}
			}
			//Stänger ner menyn och startar spelet, skickar med spelarinfon
			if (players.size() >= 1) {
				this.dispose();
				f.dispose();
				new Memory(players);
			}
		}
		//Om man trycker på hjälpknappen visas en ruta med lite instruktioner
		if (e.getSource() == help) {
			f.setVisible(true);
			f.setBounds(300, 200, 500, 200);
			f.setResizable(false);
			String text = "<html>Om inställningar:<br>Skriv in spelarnas namn för att lägga till dem.<br>" +
						  "Om AI önskas: Klicka i rutan och ange intellegens. (Antal kort AIn kommer ihåg.)<br>" +
						  "Tryck på 'Starta' för att starta spelet.<br><br>" +
						  "Hur man spelar:<br>Spelaren vars tur det är får sitt namn markerat med grön färg längst ner.<br>" +
						  "För att göra ditt drag, tryck på korten. Du kan inte välja samma kort två gånger<br><br>" +
						  "Det är ett vanligt memory, lets go!</html>";
			f.add(new JLabel(text), BorderLayout.NORTH);
		}
		for (int i = 0; i < boxes.size(); i++) {
			//Om checkboxarna är ikryssade lägger den till ett eget namn för AIn som ska skapas och visar rutan för intellegensbestämning
			if (boxes.get(i).isSelected()) {
				textfields.get(i).setText("AI " + (i+1));
				textfields.get(i).setEnabled(false);
				intfields.get(i).setText(""+20);
				intfields.get(i).setVisible(true);
			}
			//Om checkboxarna blir urklickade igen, nollställ textfälten, och ta bort rutan för intellegensbestämning
			else if (!boxes.get(i).isSelected()) {
				if (textfields.get(i).getText().indexOf("AI") != -1) {
					textfields.get(i).setText("");
					intfields.get(i).setVisible(false);
				}
				textfields.get(i).setEnabled(true);
			}
		}
	}
}
//Klassen Scorelist - skapar en meny med spelinställningar
class Scorelist extends JFrame implements ActionListener {
	private JPanel panel;
	private String[] labeltext = {"Placering", "Nick", "Score", "1", "2", "3", "4"};
	private LinkedList<Player> players = new LinkedList<Player>();
	private JButton exit = new JButton("Avsluta");
	public Scorelist(LinkedList<Player> players) {
		super("Scores");
		this.players = players;
		panel = new JPanel(new GridLayout((3 + players.size()),3));
		createLabels();
		this.add(panel);
		this.setVisible(true);
		this.setSize(300, 30 * (3 + players.size()));
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	//Metoden sortPlayers - Sorterar spelarlistan, bubblesort används då det är en liten lista
	public LinkedList<Player> sortPlayers(LinkedList<Player> tosort) {
		boolean isSorted = false;
		Player replace;
		//Kär så länge listan inte är färdigsorterad
		while (isSorted == false) {
			for (int i = 1; i < players.size(); i++) {
				if (tosort.get(i-1).score < tosort.get(i).score) {
					replace = tosort.get(i);
					tosort.remove(i);
					tosort.add(i-1, replace);
				}
			}
			//Kollar om listan är sorterad
			for (int i = 1; i < players.size(); i++) {
				if (tosort.get(i-1).score < tosort.get(i).score) {
					isSorted = false;
					break;
				}
				else {
					isSorted = true;
				}
			}
		}
		return tosort;
	}
	//Metoden createLabels - Skapar de grafiska komponenterna för scorelistan
	public void createLabels() {
		//Om det är fler än en spelare, sortera listan efter score
		if (players.size() > 1) {
			players = sortPlayers(players);
		}
		//Lägg till labels med förbestämd text
		for (int i = 0; i < 3; i++) {
			panel.add(new JLabel(labeltext[i]));
		}
		//Lägg till labels med placering, nick och poäng för spelarna
		for (int i = 3; i < 7; i++) {
			if (players.size() > i-3) {
				panel.add(new JLabel(labeltext[i]));
				panel.add(new JLabel(players.get(i-3).nick));
				panel.add(new JLabel(players.get(i-3).score + ""));
			}
		}
		//Lägger till några tomma labels i panelen för att skapa mellanrum och symetri
		for (int i = 0; i < 4; i++) {
			panel.add(new JLabel(" "));
		}
		//Lägger till avslutaknappen
		panel.add(exit);
		exit.addActionListener(this);
	}
	//Action - vad som händer om man klickar på knappen
	public void actionPerformed(ActionEvent e) {
		//Om man trycker på Avslutaknappen
		if (e.getSource() == exit) {
			//Stänger ner scorelistan och avslutar spelet
			System.exit(0);
		}
	}
}
//Klassen AIHandler Egen tråd för att styra AIn
class AIHandler extends Thread {
	public void run() {
		while (true) {
			for (int i = 0; i < Memory.players.size(); i++) {
				//Tillkalla spelmetoden, om det är ett AI gör den sitt drag, är det en spelare gör den inget.
				if (Memory.players.get(i).myTurn == true) {
					Memory.players.get(i).play();
					break;
				}
			}
			//När AIt gjort sitt drag, lägg tråden i sleep
			while (true) {
				try {
					this.sleep(1000000);
				}
				//Om tråden interruptas, breakas sleepen och börjar om från början
				catch (InterruptedException e) {
					break;
				}
			}
		}
	}
}