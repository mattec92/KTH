import java.util.*;
import java.awt.*;
import javax.swing.*;

public class Lab1_23Maze extends JFrame {
	private final int xbound;
	private final int ybound;
	private int[][] maze;

	//Konstruktor, sätter bounds, size, osv.
	public Lab1_23Maze(int xin, int yin) {
		xbound = xin;
		ybound = yin;
		maze = new int[xbound][ybound];
		createMaze(0, 0);
		solve(0 , 0, xbound-1, ybound-1, null);
		this.setVisible(true);
		this.setSize(48*xbound, 50*ybound);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setResizable(true);
	}

	//Metod som skapar en labyrint. Använder Recursive Backtracking för att skapa labyrinten.
	//Går i random riktning, om den är inom bounds och är obesökt. Kallar sedan på sig själv från den nya positionen.
	public void createMaze(int inx, int iny) {
		//Slumpar fram olika directions och går i tur och ordning till dem, om det går.
		DIR[] dirs = DIR.values();
		Collections.shuffle(Arrays.asList(dirs));
		for (DIR dir : dirs) {
			//Nästa ruta är nuvarande ruta + random direction
			int newx = inx + dir.x;
			int newy = iny + dir.y;
			if ((newx >= 0) && (newx < xbound) && (newy >= 0) && (newy < ybound) && (maze[newx][newy] == 0)) {
				//Lägg till vilket håll det saknas en vägg i nuvarande yta, lägger till bitrepresentation
				maze[inx][iny] += dir.id;
				//Lägg till vilket håll det saknas en vägg i nästa ruta, rutan man kommer till.
				maze[newx][newy] += dir.opposite.id;
				createMaze(newx, newy);
			}
		}
	}
	//Metod som löser en labyrint. Använder Recursive Backtracking som metod för att hitta en väg genom labyrinten.
	public boolean solve(int startx, int starty, int endx, int endy, DIR indir) {
		//Om positionen man är på är utanför labyrinten, returna false
		if (!(startx >= 0) && (startx < xbound) && (starty >= 0) && (starty < ybound)) {
			return false;
		}
		//Om man har gått igenom hela labyrinten, returna true
		if (startx == endx && starty == endy) {
			maze[startx][starty] |= 1024;
			return true;
		}
		DIR newdir;
		DIR[] dirs = DIR.values();
		for (DIR dir : dirs) {
			//Om det går att gå i riktningen och den valda riktningen inte är samma som man kom ifrån, gå ditåt.
			if ((maze[startx][starty] & dir.id) == dir.id && indir != dir.opposite) {
				//Markera rutan som lösning till labyrinten
				maze[startx][starty] |= 1024;
				newdir = dir;
				int newx = startx + dir.x;
				int newy = starty + dir.y;
				//Ta ett till steg i labyrinten utifrån den nya positionen
				if (solve(newx, newy, endx, endy, newdir) == true) {
					return true;
				}
			}
		}
		//Avmarkera rutan som lösning till labyrinten om vägen inte ledde till en lösning.
		maze[startx][starty] &= ~1024;
		return false;
	}

	//Enum för att hålla koll på riktningar samt id för hur gångarna går.
	private enum DIR {
		N(1, 0, -1), S(2, 0, 1), E(4, 1, 0), W(8, -1, 0);
		private final int id;
		private final int x;
		private final int y;
		private DIR opposite;

		static {
			N.opposite = S;
			S.opposite = N;
			E.opposite = W;
			W.opposite = E;
		}

		private DIR(int id, int x, int y) {
			this.id = id;
			this.x = x;
			this.y = y;
		}
	};

	//Skapar en grafisk representation av labyrinten
	public void paint(Graphics g) {
		int col, row, cellsize = 22;
		g.setColor(Color.BLACK);
		g.fillRect(0 , 0, 10000, 10000);
		for (int rowid = 0; rowid < ybound; rowid++) {
			for (int colid = 0; colid < xbound; colid++) {
				row = rowid*2*cellsize + 51;
				col = 2*colid*cellsize + 30;
				//Om aktuell cell tillhör lösningen på labyrinten, markera cellen blå.
				if ((maze[colid][rowid] & 1024) == 1024) {
					g.setColor(Color.BLUE);
					g.fillRect(col, row, cellsize, cellsize);
					//Om det finns en cell åt öster och den också är en lösning,
					//markera gången mellan aktuell cell och cellen österut blå.
					if ((maze[colid][rowid] & 4) == 4 && (maze[colid+1][rowid] & 1024) == 1024) {
						g.fillRect(col+cellsize, row, cellsize, cellsize);
					}
					else if ((maze[colid][rowid] & 4) == 4) {
						g.setColor(Color.WHITE);
						g.fillRect(col+cellsize, row, cellsize, cellsize);
					}
					//Om det finns en cell åt söder och den också är en lösning,
					//markera gången mellan aktuell cell och cellen söderut blå.
					if ((maze[colid][rowid] & 2) == 2 && (maze[colid][rowid+1] & 1024) == 1024) {
						g.setColor(Color.BLUE);
						g.fillRect(col, row+cellsize, cellsize, cellsize);
					}
					else if ((maze[colid][rowid] & 2) == 2) {
						g.setColor(Color.WHITE);
						g.fillRect(col, row+cellsize, cellsize, cellsize);
					}
				}
				//Om aktuell cell inte tillhör lösningen, markera den vit.
				else  {
					g.setColor(Color.WHITE);
					g.fillRect(col, row, cellsize, cellsize);
					//Om det finns en cell österut, markera även denna vit.
					if ((maze[colid][rowid] & 4) == 4) {
						g.fillRect(col+cellsize, row, cellsize, cellsize);
					}
					//Om det finns en cell söderut, markera även denna vit.
					if ((maze[colid][rowid] & 2) == 2) {
						g.fillRect(col, row+cellsize, cellsize, cellsize);
					}
				}
			}
		}
	}

	public static void main(String[] arg) {
		Lab1_23Maze maze = new Lab1_23Maze(30, 15);
	}
}
