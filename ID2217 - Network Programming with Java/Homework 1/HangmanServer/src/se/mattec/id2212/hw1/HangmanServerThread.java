package se.mattec.id2212.hw1;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;

public class HangmanServerThread extends Thread {

	private static final int FAILED_ATTEMPTS_ALLOWED = 10;

	private ArrayList<String> 	words;
	private Socket 				socket;

	private String 	currentWord;
	private String 	guessedCharacters;
	private int 	guessesLeft;
	private int 	score;

	public HangmanServerThread(ArrayList<String> words, Socket socket) {
		this.words = words;
		this.socket = socket;
		this.score = 0;
	}

	/**
	 * Message format:
	 * wordWithDashes , message , guessesLeft , Score
	 * */
	@Override
	public void run() {
		super.run();

		try {
			PrintWriter writer = new PrintWriter(socket.getOutputStream());
			BufferedReader reader = new BufferedReader( new InputStreamReader(socket.getInputStream()));

			String str;
			while ((str = reader.readLine()) != null) {
				if (str.equals("start game")) {
					//Game is starting, reset values and choose a random word.
					currentWord = getRandomWord();
					guessesLeft = FAILED_ATTEMPTS_ALLOWED;
					guessedCharacters = "";

					String message = String.format("%s,Guess the word!,%d,%d", getWordWithDashes(), guessesLeft, score);
					writer.println(message);
					writer.flush();
				}
				else if (str.equals(currentWord)) {
					//Correct word is guessed
					score++;
					String message = String.format("%s,Congratulations! You won!,%d,%d", currentWord, guessesLeft, score);
					writer.println(message);
					writer.flush();
				}
				else if (str.length() == 1) {
					//One character is guessed
					String oldWordWithDashes = getWordWithDashes();
					guessedCharacters += str;
					String newWordWithDashes = getWordWithDashes();

					if (newWordWithDashes.equals(currentWord)) {
						//Word is completed
						score++;
						String message = String.format("%s,Congratulations! You won!,%d,%d", currentWord, guessesLeft, score);
						writer.println(message);
						writer.flush();
					}
					else if (oldWordWithDashes.equals(newWordWithDashes)) {
						//Guess was wrong
						guessesLeft--;

						if (guessesLeft == 0) {
							//Game over
							score--;
							String message = String.format("%s,Game over!,%d,%d", currentWord, guessesLeft, score);
							writer.println(message);
							writer.flush();
						}
						else {
							//Game goes on, with one less guess left
							String message = String.format("%s,Guess was wrong!,%d,%d", newWordWithDashes, guessesLeft, score);
							writer.println(message);
							writer.flush();
						}
					}
					else {
						//Guess was correct
						String message = String.format("%s,Guess was correct!,%d,%d", newWordWithDashes, guessesLeft, score);
						writer.println(message);
						writer.flush();
					}
				}
				else {
					//Guessing the whole word wrong
					guessesLeft--;
					
					if (guessesLeft == 0) {
						//Game over
						score--;
						String message = String.format("%s,Game over!,%d,%d", currentWord, guessesLeft, score);
						writer.println(message);
						writer.flush();
					}
					else {
						//Game goes on, with one less guess left
						String message = String.format("%s,Guess was wrong!,%d,%d", getWordWithDashes(), guessesLeft, score);
						writer.println(message);
						writer.flush();
					}
				}
			}

			socket.close();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Get a random word from the list.
	 * */
	private String getRandomWord() {
		int index = (int) (Math.random() * words.size());
		return words.get(index).toLowerCase();
	}

	/**
	 * Generate a dashed word from the guesses made.
	 * */
	private String getWordWithDashes() {
		if (guessedCharacters.length() > 0) {
			return currentWord.replaceAll("[^" + guessedCharacters + "]", "-");
		}
		else {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < currentWord.length(); i++) {
				sb.append("-");
			}
			return sb.toString();
		}
	}
}
