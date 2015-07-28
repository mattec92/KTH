package se.mattec.id2212.hw1;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Frame;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import javax.swing.BoxLayout;

public class HangmanClient {
	private static PrintWriter writer;
	
	private static Label scoreLabel;
	private static Label guessesLeftLabel;
	private static Label messageLabel;
	private static Label wordLabel;

	public static void main(String[] args) {
		String hostName = "localhost";
		int port = 4444;
		
		if (args != null && args.length > 0 ) {
			hostName = args[0];
			
			if (args.length > 1) {
				try {
					port = Integer.parseInt(args[1]);
				}
				catch (NumberFormatException e) {
					System.out.println("Usage: java HangmanServer [PORTNR (int)]\nWill use standard port " + port);
				}
			}
		}
		
		setupGUI();

		//Setup socket and start receiver thread.
		try {
			Socket socket = new Socket(hostName, port);

			new ReceiverThread(socket).start();

			writer = new PrintWriter(socket.getOutputStream());

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Helper to send a message.
	 * */
	private static void sendMessage(String message) {
		writer.println(message);
		writer.flush();
	}

	/**
	 * Initialize the GUI
	 * */
	private static void setupGUI() {
		scoreLabel = new Label("Score: --");
		guessesLeftLabel = new Label("Guesses left: --");
		messageLabel = new Label("");
		wordLabel = new Label("");

		final TextField textField = new TextField();
		textField.setColumns(30);

		Button startButton = new Button("New game");
		startButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent arg0) {
				sendMessage("start game");
				textField.requestFocus();
			}
		});

		Button sendButton = new Button("Send");
		sendButton.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent arg0) {
				String textFromTextField = textField.getText();

				if (textFromTextField.length() > 0) {
					textField.setText("");
					sendMessage(textFromTextField);
				}

				textField.requestFocus();
			}
		});
		
		Panel topPanel = new Panel();
		topPanel.add(scoreLabel);
		topPanel.add(guessesLeftLabel);
		topPanel.add(startButton);

		Panel middlePanel = new Panel();
		middlePanel.setLayout((new BoxLayout(middlePanel, BoxLayout.PAGE_AXIS)));
		middlePanel.add(messageLabel);
		middlePanel.add(wordLabel);

		Panel bottomPanel = new Panel();
		bottomPanel.add(textField);
		bottomPanel.add(sendButton);
		
		Panel panel = new Panel(new BorderLayout());
		panel.add(topPanel, BorderLayout.NORTH);
		panel.add(middlePanel, BorderLayout.CENTER);
		panel.add(bottomPanel, BorderLayout.SOUTH);

		Frame frame = new Frame("Hangman");
		frame.add(panel);
		frame.pack();
		frame.setVisible(true);

		frame.addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we){
				System.exit(0);
			}
		});
	}

	/**
	 * Message format:
	 * wordWithDashes , message , guessesLeft , Score
	 * */
	static class ReceiverThread extends Thread {
		private Socket socket;

		public ReceiverThread(Socket socket) {
			this.socket = socket;
		}

		@Override
		public void run() {
			super.run();
			try {
				BufferedReader reader = new BufferedReader( new InputStreamReader(socket.getInputStream()));

				String str;
				while ((str = reader.readLine()) != null) {
					//Read messages of specified format. Update labels.
					String[] message = str.split(",");

					wordLabel.setText(message[0]);
					messageLabel.setText(message[1]);
					guessesLeftLabel.setText("Guesses left: " + message[2]);
					scoreLabel.setText("Score: " + message[3]);
				}
				socket.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
