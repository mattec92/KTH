import java.io.*;
import java.net.*;
import java.util.*;

/**
   @author Mattias Cederlund, mcede@kth.se
*/
public class ATMServerThread extends Thread {
	private ArrayList<User> users;
	private String motd;
    private Socket socket = null;
    private BufferedReader in;
    PrintWriter out;
    User userLoggedIn = null;
    
    public ATMServerThread(Socket socket, ArrayList<User> users, String motd) {
        super("ATMServerThread");
        this.socket = socket;
        this.users = users;
        this.motd = motd;
    }
    
    //Set the servers MOTD and push it to the client.
    public void setMotd(String in) {
    	motd = in;
    	sendMOTD();
    }
    
    //Runs through the log-in sequence.
    public void validateUser() throws IOException{
    	boolean loggedIn = false;
    	//Ask the client to log in.
		out.println(0); 
		while (loggedIn == false) {
			//Ask the client to enter a card number.
			out.println(1);
			try {
				//Wait for a response.
				int userNr = Integer.parseInt(receive());
				for (int i = 0; i < users.size(); i++) {
					//Checks if the entered card number is valid.
					if (users.get(i).getCardNr() == userNr) {
						//If so, ask the client to enter the pin-code.
						out.println(2);
						//Wait for a response.
						int userCode = Integer.parseInt(receive());
						//Checks if the provided pin-code matches the pin code of the card.
						if (users.get(i).login(userCode) == true) {
							//If so, send a confirmation that the pin-code was correct and exit the log-in sequence.
							out.println(3);
							userLoggedIn = users.get(i);
							loggedIn = true;
						}
						break;
					}
				}
				//If the provided card number or pin-code is incorrect, ask the client to try again.
				if (loggedIn == false) {
					out.println(4);
				}
			}
			//If the clients input was unexpected (not integers), ask the client to try again.
			catch (NumberFormatException e) {
				out.println(5);
			}
		}
    }
    
    //Sends the MOTD to the client.
    public void sendMOTD() {
    	//Send the code 20 to the client so it will prepare to receive the MOTD, then the MOTD.
    	out.println("20");
    	out.println(motd);
    }
    
    //Sends a language-pack consisting of a String-array to the client.
    public void sendLanguage(String[] language) {
    	//Send the code 30 to the client so it will prepare to receive a language-pack.
    	out.println(30);
    	//Send all the Strings contained in the array to the client.
    	for (int i = 0; i < language.length; i++) {
    		out.println(language[i]);
    	}
    }
    
    
    	//The method for sending data.
    	public void send(String toSend) {
			//If the data to be sent is longer than 5 characters the data must be split.
    		if (toSend.length() > 5) {
    			//If so mark the beginning of the data with a single %.
    			out.println("%");
    			int i;
    			//Then split the data in pieces of max 5 characters and send them.
    			for (i = 0; i+4 < toSend.length(); i += 5) {
    				out.println(toSend.substring(i,i+5));
    			}
    			out.println(toSend.substring(i, toSend.length()));
    			//When all data is sent, send another single % to mark the end of the transmission.
    			out.println("%");
    		}
    		else {
    			out.println(toSend);
    		}
    	}
	
	//The method for receiving potentially long input from the server.
	public String receive() throws IOException {
		String received = in.readLine();
		//If the received data is a single %.
		if (received.equals("%")) {
			String out = "";
			String part = in.readLine();
			//Build the output String from the parts untill a single % is found, then return.
			while (part.equals("%") == false) {
				out += part;
				part = in.readLine();
			}
			return out;
		}
		//If the data was not a single %, return the data.
		else {
			return received;
		}
	}
	
    public void run(){
        try {
        	//Create in- and out-streams.
            out = new PrintWriter(socket.getOutputStream(), true);
            in = new BufferedReader
                (new InputStreamReader(socket.getInputStream()));
                
            //Try to validate the user.
            validateUser();
			
			//If validate succeeded, welcome the client to the private menu.
            out.println(6); 
            //Send the MOTD to the client along with the menu options.
			sendMOTD();
            boolean exit = false;
            int choice = 0;
            //The main-loop of the ATMServerThread, loops until the user wants to end the session. 
            while(exit == false) { 
				//Show the MOTD and menu to the client.
				out.println(21);
            	out.println(7);
            	try {
            		//Wait for a choice of options from the client
            		choice = Integer.parseInt(receive());
	            	switch (choice) {
	            		//If the client chose to view the balance, send the balance to the client.
	            		case 1:
	            			out.println(8);
	            			send("" +userLoggedIn.getBalance());
	            			break;
	            		//If the client wishes to make a withdrawal, ask for how much.
	            		case 2:
	            			out.println(9);
	            			//Wait for an answer and then ask for the security code.
	            			int wamount = Integer.parseInt(receive());
	            			out.println(10);
	            			//Wait for an answer and then try to withdraw from the account.
	            			int code = Integer.parseInt(receive());
	            			//If the security code matches tell the client the withdrawal was successfull.
	            			if (userLoggedIn.withdraw(wamount, code)) {
	            				out.println(11);
	            				send("" + wamount);
	            				//Remind the client of the new account balance.
	            				out.println(12);
	            				send("" + userLoggedIn.getBalance());
	            			}
	            			else {
	            				//If the security code did not match, let the client know.
	            				out.println(13);
	            			}
	            			break;
	            		//If the client wishes to make a deposit, ask for how much.
	            		case 3:
	            			out.println(14);
	            			//Wait for an answer and deposit the desired amount to the account.
	            			int damount = Integer.parseInt(receive());
	            			userLoggedIn.deposit(damount);
	            			//Remind the client of the new account balance.
	            			out.println(15);
	            			send("" + userLoggedIn.getBalance());
	            			break;
	            		//If the client wishes to exit, send a good bye message, which will cause the client to terminate.
	            		case 4:
	            			out.println(16);
	            			exit = true;
	            			break;
	            		//If the client wants a new language, call sendLanguage.
	            		case 5:
	            			sendLanguage(newLang);
	            			break;
	            		//If input did not match any of the above, send an error message and ask the client to try again.
	            		default:
	            			out.println(17);
	            			break;
	            	}
            	}
            	//If the clients input was unexpected (not integers), ask the client to try again. 
            	catch (NumberFormatException e) {
            		out.println(5);
            	}
            }
            //Terminate the cleints, close all the streams and the socket.
            out.println(16);
            out.close();
            in.close();
            socket.close();
        }catch (IOException e){
            e.printStackTrace();
        }
    }
    //Array of new language.
	String[] newLang = {"Vänligen logga in",
						"Kortnummer:",
						"PIN-kod:",
						"Inloggningen lyckades",
						"Något fel. Försök igen",
						"Oväntad input. Försök igen",
						"Välkommen till Bankens privata meny!",
						"(1)Saldo, (2)Uttag, (3)Insättning, (4)Avsluta, (5)Switch language to english",
						"Saldot är: $",
						"Skriv in hur mycket som ska tas ut",
						"Skriv in säkerhetskoden för det här uttaget",
						"Uttaget lyckades. Tog ut: $",
						"Nytt saldo efter uttag är: $",
						"Uttag misslyckades. Fel kod.",
						"Skriv in hur mycket som ska sättas in: ",
						"Nytt saldo efter insättning är: $",
						"Hej då!",
						"Inte ett giltigt alternativ. Försök igen."};
}
