import java.net.*;
import java.io.*;
import java.util.*;

/**
   @author Mattias Cederlund, mcede@kth.se
*/
public class ATMServer {
	private static int connectionPort = 8989;
	private static String motd = "Welcome to teh Bank! This is the standard MOTD.";
    private static ArrayList<ATMServerThread> sessions = null;
    private static ArrayList<User> users = null;
    
    public static void main(String[] args) throws IOException {
    	sessions = new ArrayList<ATMServerThread>();
    	Scanner userscanner = new Scanner(new FileInputStream("users.txt"));
    	users = new ArrayList<User>();
    	
    	//Retrive user info from file and load them into the bank program.
    	while (userscanner.hasNext()) {
    		users.add(new User(userscanner.nextInt(), userscanner.nextInt(), 
    					userscanner.nextInt(), userscanner.nextInt()));
    	}

        ServerSocket serverSocket = null;
       	boolean listening = true;
       	
        try {
            serverSocket = new ServerSocket(connectionPort); 
        } 
        catch (IOException e) {
            System.err.println("Could not listen on port: " + connectionPort);
            System.exit(1);
        }
		System.out.println("Bank started listening on port: " + connectionPort);
		
		//Start new thread to handle input in the bank console.
		new BankInputThread(motd).start();
		
        while (listening) {
        	//Start a new session and add it to the list of sessions.
        	//The list of users and the current MOTD is transfered to the new session-threads.
            sessions.add(new ATMServerThread(serverSocket.accept(), users, motd));
       	    sessions.get(sessions.size()-1).start();
        }
        serverSocket.close();
    }
    
    //The thread that handles input to the server console.
    static class BankInputThread extends Thread {
   		Scanner sc;
   		public BankInputThread(String motd) {
    		super("BankInputThread");
    	}
    	
    	public void run() {
    		sc = new Scanner(System.in);
    		String choice;
    		while (true) {
    			//Show the menu and wait for a choice.
    			System.out.println("Bank menu: (1)Set new MOTD, (2)Show all users, " +
    								"(3)Save user data and quit");
    			choice = sc.nextLine();
    			switch (choice) {
    				case "1":
    					//Wait for a new MOTD.
    					System.out.println("Type in a new MOTD if wanted.");
    					String newMOTD = sc.nextLine();
    					//If the new MOTD is not longer than 80 characters set the current motd to the new one.
    					if (newMOTD.length() <= 80) {
    						motd = newMOTD;
    						System.out.println("New motd: " + motd);
    						//Set the new motd in all sessions.
	    					for (int i = 0; i < sessions.size(); i++) {
	    						sessions.get(i).setMotd(motd);
	    					}
    					}
    					//If too long, show an error message.
    					else {
    						System.out.println("MOTD too long. Try again.");
    					}
    					break;
    				case "2":
    					//Print all user information with the users built-in toString().
    					for (int i = 0; i < users.size(); i++) {
	    					System.out.println(users.get(i));
	    				}
	    				break;
    				case "3":
    					try {
    						//Print all the users information to a text file and terminate the server.
	    					PrintWriter out = new PrintWriter(new FileWriter("users.txt"));
	    					for (int i = 0; i < users.size(); i++) {
		    					out.println(users.get(i).export());
		    				}
		    				out.close();
		    				System.exit(1);
    					}
    					catch (IOException e) {
    						e.printStackTrace();
    					}
    				default:
    					break;
    			}

    		}
    	}
   	}
}