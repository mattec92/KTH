/*
 * Homework 1
 * Mattias Cederlund
 * mcede@kth.se
 *
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Date;
import java.util.concurrent.locks.ReentrantLock;


public class PalindromicWordFinder {
	private static String[] 		words = new String[25143];
	private static int[] 			wordIndexes = new int[25143];
	private static int[] 			numPalindromsFoundPerWorker;
	private static int 				numWorkerThreads = 8;
	private static int 				numWorkerThreadsFinished = 0;
	
	private static ReentrantLock 	lock = new ReentrantLock();

	public static void main (String[] args) {
		//Read commandline arguments for worker-count, if there are any
		if (args.length > 0 && args[0] != null) {
			numWorkerThreads = Integer.parseInt(args[0]);
		}
		numPalindromsFoundPerWorker = new int[numWorkerThreads];
		
		//Import list of words
		readWordList();
		
		//Calculate words per worker, used to balance the work between the worker threads
		int wordsPerWorker = (int) words.length / numWorkerThreads;
		
		long startTime = new Date().getTime();
		
		//Start worker threads, assigned to a section of the list
		for (int i = 0, j = 0; j < numWorkerThreads; i += wordsPerWorker, j++) {
			//If this is the last worker, make sure to assign the whole rest of the list
			if (i == (numWorkerThreads - 1)) {
				new Worker("Worker " + j, j, i, words.length - 1).start();
			}
			else {
				new Worker("Worker " + j, j, i, i + wordsPerWorker - 1).start();
			}
		}
		
		//Wait for worker threads to finnish (Basicly a counter barrier where only main thread continues)
		while (numWorkerThreadsFinished != numWorkerThreads) {
			try {
				Thread.sleep(10);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
		long endTime = new Date().getTime();
		
		//Write result output
		writePalindromeList();
		
		System.out.println("Execution time (ms): " + (endTime - startTime));
	}

	public static void readWordList() {
		try {
			BufferedReader r = new BufferedReader(new FileReader("words.txt"));
			String line = "";
			int count = 0;
			//Read all words from the list and add them to an array
			while ((line = r.readLine()) != null) {
				words[count] = line;
				count++;
			}
			r.close();
		} 
		catch (FileNotFoundException e) {
			e.printStackTrace();
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void writePalindromeList() {
		try {
			BufferedWriter w = new BufferedWriter(new PrintWriter("palindromes.txt"));
			//Write all palindroms to a file
			int totalPalindromes = 0;
			for (int i = 0; i < words.length; i++) {
				if (wordIndexes[i] > 0) {
					w.write(words[i] + "\n");
					totalPalindromes++;
				}
			}
			
			System.out.println("Palindromes found total: " + totalPalindromes);
			
			for (int i = 0; i < numWorkerThreads; i++) {
				System.out.println("Palindromes found by worker " + i + ": " + numPalindromsFoundPerWorker[i]);
			}
		
			w.close();
		} 
		catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	//Worker thread
	static class Worker extends Thread {
		private int numPalindromesFound = 0;
		private int workerIndex;
		private int startIndex;
		private int stopIndex;

		public Worker(String name, int workerIndex, int startIndex, int stopIndex) { 
			super(name);
			this.workerIndex = workerIndex;
			this.startIndex = startIndex;
			this.stopIndex = stopIndex;
		}

		public void run() {
			//Loop through all the assigned words
			for (int i = startIndex; i <= stopIndex; i++) {
				//Get the reverse string
				String reverse = new StringBuilder(words[i]).reverse().toString();
				//Compare it to the original string, if they are the same mark position and increase counter
				if (reverse.equalsIgnoreCase(words[i])) {
					wordIndexes[i] = 1;
					numPalindromesFound++;
				}
				//If word is not palindrome of itself, compare to the rest of the whole list
				else {
					for (int j = 0; j < words.length; j++) {
						//Compare it to the original string, if they are the same mark position and increase counter
						if (reverse.equalsIgnoreCase(words[j])) {
							wordIndexes[i] = 1;
							numPalindromesFound++;
							//Break because there are no more matches to be found
							break;
						}
					}
				}
			}
			//Critical section when arriving at barrier, increasing the count
			lock.lock();
			numWorkerThreadsFinished++;
			lock.unlock();
			//Also put number of palindroms found in an array to be shown later, when result is printed
			numPalindromsFoundPerWorker[workerIndex] = numPalindromesFound;
		}

	}
}
