import java.io.*;
import java.net.*;
import java.util.*;

/**
   @author Mattias Cederlund, mcede@kth.se
*/
public class ATMServerThread extends Thread {
	private ArrayList<User> users;
	private String motd = "";
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
    public void setMotd(String in) {
    	motd = in;
    }
    
    public void validateUser() throws IOException{
    	boolean loggedIn = false;
		out.println("Please log in.");
		while (loggedIn == false) {
			out.println("Card number:");
			try {
				int userNr = Integer.parseInt(in.readLine());
				for (int i = 0; i < users.size(); i++) {
					if (users.get(i).getCardNr() == userNr) {
						out.println("Code:");
						int userCode = Integer.parseInt(in.readLine());
						if (users.get(i).login(userCode) == true) {
							out.println("Login successful.");
							userLoggedIn = users.get(i);
							loggedIn = true;
						}
						break;
					}
				}
				if (loggedIn == false) {
					out.println("Something wrong. Try again.");
				}
			}
			catch (NumberFormatException e) {
				out.println("Unexpected input. Try again.");
			}
		}
    }

    public void run(){
        try {
            out = new PrintWriter(socket.getOutputStream(), true);
            in = new BufferedReader
                (new InputStreamReader(socket.getInputStream()));
                
            validateUser();
			
            out.println("Welcome to teh Banks private menu!"); 
            boolean exit = false;
            int choise = 0;
            while(exit == false) { //Användbart om man vill skapa log-out-funktion
            	out.println(motd);
            	out.println("(1)Balance, (2)Withdrawal, (3)Deposit, (4)Exit");
            	try {
            		choise = Integer.parseInt(in.readLine());
	            	switch (choise) {
	            		case 1:
	            			out.println("Account balance is: " + userLoggedIn.getBalance() + "  dollars");
	            			break;
	            		case 2:
	            			out.println("Enter amount to withdraw: ");
	            			int wamount = Integer.parseInt(in.readLine());
	            			out.println("Enter security code for this withdrawal: ");
	            			int code = Integer.parseInt(in.readLine());
	            			if (userLoggedIn.withdraw(wamount, code)) {
	            				out.println("Withdrawal of " + wamount + " dollars succeeded.");
	            				out.println("New account balance is: " + userLoggedIn.getBalance() + "  dollars");
	            			}
	            			else {
	            				out.println("Withdrawal failed. Wrong code.");
	            			}
	            			break;
	            		case 3:
	            			out.println("Enter amount to deposit: ");
	            			int damount = Integer.parseInt(in.readLine());
	            			userLoggedIn.deposit(damount);
	            			out.println("New account balance is: " + userLoggedIn.getBalance() + "  dollars");
	            			break;
	            		case 4:
	            			out.println("Good bye!");
	            			break;
	            		default:
	            			out.println("Not a valid choise. Try again.");
	            			break;
	            	}
            	}
            	catch (NumberFormatException e) {
            		out.println("Unexpected input. Try again.");
            	}
            }
            out.println("Good bye!");
            out.close();
            in.close();
            socket.close();
        }catch (IOException e){
            e.printStackTrace();
        }
    }
}
