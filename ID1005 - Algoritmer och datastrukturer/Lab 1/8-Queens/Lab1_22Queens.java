import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class Lab1_22Queens extends JFrame implements ActionListener {
	JPanel boardpanel = new JPanel(new GridLayout(8, 8));
	LinkedList<JLabel> board = new LinkedList<JLabel>();
	JButton button = new JButton("Visa ny lösning");
	boolean run = false;

	public Lab1_22Queens() {
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(button);
		button.addActionListener(this);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setSize(350, 383);
		this.setVisible(true);
		this.setJMenuBar(menuBar);
		//Skapar schack-brädet
		for (int i = 0; i < 64; i++) {
			//Bestämmer om det är en rad som börjar med svart eller vit ruta
			//Placerar sedan ut rutor med rätt färg
			int row = (i / 8) % 2;
			board.add(new JLabel());
			if (row == 0) {
				board.get(i).setBackground(i % 2 == 0 ? Color.WHITE : Color.BLACK);
			}
			else {
				board.get(i).setBackground(i % 2 == 0 ? Color.BLACK : Color.WHITE);
			}
			board.get(i).setOpaque(true);
			boardpanel.add(board.get(i));
		}
		this.add(boardpanel);
		//Anropa metoden som hittar möjliga placeringar för drottningarna
		setQueen(new int[8], 8, 0);
	}
	//ActionEvent för att lyssna på knappen, om den trycks sätt run = true
	//Detta tillåter att man genererar en ny placering för drottningarna
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == button) {
			run = true;
		}
	}
	//Sätter drottningar på rätt plats i det grafiska schackbrädet
	public void showQueens(int[] placing) {
		System.out.println(Arrays.toString(placing));
		//Tar bort de gamla drottningarna
		for(int i = 0; i < 64; i++) {
			board.get(i).setIcon(null);
		}
		//Placerar ut nya drottningar på rätt plats
		for (int i = 0; i < 8; i++) {
			board.get(i*8+placing[i]).setIcon(new ImageIcon("queen.gif"));
		}
	}

	public boolean setQueen(int[] rows, int maxcol, int row) {
		//i, kolumner, talen i arrayen representerar kolumner
		for (int i = 0; i < maxcol; i++) {
			boolean valid = true;
			//j, rader, indexet i arrayen representerar rader
			for (int j = 0; j < row; j++) {
				//Om det finns en annan drottning på raden, kan drottning ej placeras där
				if (i == rows[j]) {
					valid = false;
				}
				else {
					//Offset = rad-kolumn, identifierar vilken diagonal man befinner sig på
					int offset = row - j;
					//Annars om det finns en drottning på diagonalen, kan den ej heller placeras där
					//Om det på någon rad finns en drottning vars kolumnindex är samma som kolumnindexet man försöker placera en dam på
					//plus eller minus diagonal-offsetet, är drottningarna på samma rad och det går alltså ej att placera en drottning där.
					if (rows[j] == (i + offset) || rows[j] == (i - offset)) {
						valid = false;
					}
				}
			}
			//Om drottningen är ensam på sin kolumn, och diagonal, placera drottningen
			rows[row] = i;
			//Om det inte finns en drottning på varje rad, placera ut ytterligare en
			if(valid && (row + 1) < rows.length) {
				valid = setQueen(rows, maxcol, (row + 1));
			}
			//Ett resultat har hittats och alla drottningar är utplacerade, visa grafisk representation
			if (valid) {
				showQueens(rows);
				//Vänta på knapptryckning för att börja producera nästa placering
				while (run == false) {
					try {
						Thread.sleep(1);
					}
					catch (InterruptedException e) {
					}
				}
				run = false;
			}
		}
		//Om inga möjliga placeringar finns, returnera false
		return false;
	}

	public static void main(String[] arg) {
		new Lab1_22Queens();
	}
}