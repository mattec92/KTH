package se.mattec.id2209.hw3.queens;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.UnreadableException;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class QueenAgent extends Agent {
	private int row;
	private int n;

	@Override
	protected void setup() {
		super.setup();

		//Get the arguments, this queens row and the total number of rows.
		Object[] arguments = getArguments();
		if (arguments.length > 0) {
			row = (int) arguments[0];
			n = (int) arguments[1];
		}
		
		System.out.println(getLocalName() + ": Started");

		//Add behaviour receiving messages.
		addBehaviour(new MessageReceiverBehaviour());

		//If this is the first queen, start the placement finding algorithm.
		if (row == 0) {
			System.out.println(getLocalName() + ": Is first queen. Starting algorithm.");
			int[] rows = new int[n];
			Arrays.fill(rows, 0);
			findPlacement(rows, 0);
		}

	}

	/**
	 * Behaviour for receiving messages from other queen agents.
	 * */
	class MessageReceiverBehaviour extends CyclicBehaviour {

		@Override
		public void action() {
			ACLMessage message = receive();

			if (message != null) {
				//Retryoffset is used to move the current queen forward one row, if it is a re-positioning.
				int retryOffset = 0;
				
				switch (message.getPerformative()) {
				case ACLMessage.FAILURE: //Informed of failure, replace current queen.
					retryOffset = 1;
				case ACLMessage.INFORM: //Informed of successful placement
					int[] rows = null;

					try {
						rows = (int[]) message.getContentObject();
					} 
					catch (UnreadableException e) {
						e.printStackTrace();
					}

					findPlacement(rows, retryOffset);
					break;

				default:
					break;
				}
			}
			else {
				block();
			}
		}

	}

	/**
	 * Sends a message to notify the next so it will position itself.
	 * */
	private void notifyNextQueen(int[] rows) {
		ACLMessage message = new ACLMessage(ACLMessage.INFORM);
		message.addReceiver(new AID("q" + (row + 1), AID.ISLOCALNAME));

		try {
			message.setContentObject(rows);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}

		send(message);
	}

	/**
	 * Sends a message to the previous queen to re-position itself.
	 * */
	private void notifyPreviousQueen(int[] rows) {
		ACLMessage message = new ACLMessage(ACLMessage.FAILURE);
		message.addReceiver(new AID("q" + (row - 1), AID.ISLOCALNAME));

		try {
			message.setContentObject(rows);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}

		send(message);
	}

	/**
	 * Algorithm for finding a placement.
	 * */
	private void findPlacement(int[] rows, int retryOffset) {

		//i, what column the queen should be placed in
		for (int i = rows[row] + retryOffset; i < rows.length; i++) {
			boolean isValid = true;
			//j, rows. Loop through all rows of previous placed queens.
			for (int j = 0; j < row; j++) {
				//If there is another row with a queen placed on column i, the placement is invalid.
				if (i == rows[j]) {
					isValid = false;
					break;
				}
				else {
					//Offset = row - column. Identifies the diagonal.
					int offset = row - j;
					//If there is a queen with the same column index (value in array) as the one
					//being placed +- diagonal offset, the placement is not valid.
					if (rows[j] == (i + offset) || rows[j] == (i - offset)) {
						isValid = false;
						break;
					}
				}
			}

			//If it's a valid placement, update the array and notify next queen (or build gui)
			if (isValid) {
				rows[row] = i;

				//If this is the last row, show placement.
				if (row == (rows.length - 1)) {
					System.out.println(getLocalName() + ": Solution found " + Arrays.toString(rows));
					buildGUI(rows);
				}
				//Otherwise notify the next queen.
				else {
					rows[row + 1] = 0;
					notifyNextQueen(rows);
				}

				return;
			}
		}

		//If this is the first queen. There does not exist any more solutions.
		if (row == 0) {
			System.out.println(getLocalName() + ": No more solutions exists.");
		}
		//Otherwise, notify previous queen to make a re-placement.
		else {
			notifyPreviousQueen(rows);
		}

	}
	
	/**
	 * Builds a graphical presentation of the queens positions.
	 * */
	private void buildGUI(final int[] rows) {
		final JFrame frame = new JFrame();
		JPanel panel = new JPanel(new GridLayout(rows.length, rows.length));
		ArrayList<JLabel> board = new ArrayList<JLabel>();
		JButton button = new JButton("Generate next solution");
		
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(button);
		button.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				notifyPreviousQueen(rows);
				frame.dispose();
			}
		});
		
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setSize((int) (43.75 * rows.length), (int) (43.75 * rows.length + 33));
		frame.setVisible(true);
		frame.setJMenuBar(menuBar);
		
		for (int i = 0; i < (rows.length * rows.length); i++) {
			board.add(new JLabel());
			if (rows.length % 2 == 0) { //For even numbers, switch black/white depending on row
				int rowIndex = (i / rows.length) % 2;
				if (rowIndex == 0) {
					board.get(i).setBackground(i % 2 == 0 ? Color.WHITE : Color.BLACK);
				}
				else {
					board.get(i).setBackground(i % 2 == 0 ? Color.BLACK : Color.WHITE);
				}
			}
			else { //For uneven, keep doing each other
				board.get(i).setBackground(i % 2 == 0 ? Color.WHITE : Color.BLACK);
			}
			board.get(i).setOpaque(true);
			panel.add(board.get(i));
		}
		
		for (int i = 0; i < rows.length; i++) {
			board.get(i*rows.length+rows[i]).setIcon(new ImageIcon("queen.gif"));
		}
		
		frame.add(panel);
	}

}
