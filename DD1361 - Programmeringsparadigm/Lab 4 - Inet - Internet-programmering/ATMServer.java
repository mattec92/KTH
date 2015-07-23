import java.net.*;
import java.io.*;
import java.util.*;

/**
   @author Mattias Cederlund, mcede@kth.se
*/
public class ATMServer {
	private static int connectionPort = 8989;
	static String motd = "Welcome to teh Bank! This is the standard MOTD.";
    static ArrayList<ATMServerThread> sessions = null;
    
    public static void main(String[] args) throws IOException {
    	sessions = new ArrayList<ATMServerThread>();
    	
    	ArrayList<User> users = new ArrayList<User>();
    	users.add(new User(1, 1, 1000));
    	users.add(new User(2, 2, 2000));
    	users.add(new User(3, 3, 3000));
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
		
		new BankInputThread(motd).start();
		
        while (listening) {
            sessions.add(new ATMServerThread(serverSocket.accept(), users, motd));
       	    sessions.get(sessions.size()-1).start();
        }
        serverSocket.close();
    }
    
    static class BankInputThread extends Thread {
   		Scanner sc;
   		public BankInputThread(String motd) {
    		super("BankInputThread");
    	}
    	
    	public void run() {
    		sc = new Scanner(System.in);
    		String temp = "";
    		while (true) {
    			System.out.println("Type in a new MOTD if wanted.");
    			temp = sc.nextLine();
    			if (temp.length() <= 80) {
    				motd = temp;
	    			System.out.println("New motd: " + motd);
	    			for (int i = 0; i < sessions.size(); i++) {
	    				sessions.get(i).setMotd(motd);
	    			}
    			}
    			else {
    				System.out.println("MOTD too long.");
    			}
    		}
    	}
   	}
}