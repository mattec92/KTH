package se.mattec.id2212.hw1;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;

public class HangmanServer {
	private static ArrayList<String> words;


	public static void main(String[] args) {
		int port = 4444;
		
		//Read port nr from arguments
		if (args != null && args.length > 0) {
			try {
				port = Integer.parseInt(args[0]);
			}
			catch (NumberFormatException e) {
				System.out.println("Usage: java HangmanServer [HOSTNAME] [PORTNR (int)]\nWill use standard port " + port);
			}
		}

		readWordList();

		//Setup server socket and start server threads if someone connects.
		try {
			ServerSocket serverSocket = new ServerSocket(port);

			while (true) {
				Socket clientSocket = serverSocket.accept();

				HangmanServerThread serverThread = new HangmanServerThread(words, clientSocket);

				serverThread.start();
				
				System.out.println("Client connected.");
			}

			//serverSocket.close(); //Unreachable. Never stopping server.

		} 
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Read word list from file.
	 * */
	public static void readWordList() {
		words = new ArrayList<String>();

		try {
			BufferedReader reader = new BufferedReader(new FileReader("words.txt"));
			String line;
			//Read all words from the list and add them to an array
			while ((line = reader.readLine()) != null) {
				words.add(line);
			}
			reader.close();
		} 
		catch (FileNotFoundException e) {
			e.printStackTrace();
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
	}

}
