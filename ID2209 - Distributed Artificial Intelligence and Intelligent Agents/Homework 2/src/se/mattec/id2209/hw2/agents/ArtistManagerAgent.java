package se.mattec.id2209.hw2.agents;

import jade.core.Agent;

import java.awt.Button;
import java.awt.Frame;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import se.mattec.id2209.hw2.behaviours.AuctioneerBehaviour;
import se.mattec.id2209.hw2.models.Artifact;

@SuppressWarnings("serial")
public class ArtistManagerAgent extends Agent {

	@Override
	protected void setup() {
		super.setup();
		System.out.println("ArtistManagerAgent started, name: " + getLocalName());

		setupGUI();
	}

	/**
	 * Creates a simple gui for starting auctions.
	 * */
	private void setupGUI() {
		final Frame frame = new Frame("Auction");

		Panel panel = new Panel();

		final TextField textField = new TextField();
		textField.setColumns(10);

		Button button = new Button("Start auction");

		//When the start button is clicked an auction is started (using AuctioneerBehaviour)
		button.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent arg0) {
				try {
					int price = Integer.parseInt(textField.getText());
					new AuctioneerBehaviour(ArtistManagerAgent.this, new Artifact(), price);
				}
				catch (NumberFormatException e) {
					System.out.println("Not a valid price");
					e.printStackTrace();
				}
			}
		});

		panel.add(textField);
		panel.add(button);

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we){
				frame.dispose();
			}
		});
	}

}
