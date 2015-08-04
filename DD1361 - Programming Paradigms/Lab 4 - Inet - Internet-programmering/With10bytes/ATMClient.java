import java.io.*;   
import java.net.*;  
import java.util.Scanner;

/**
   @author Mattias Cederlund, mcede@kth.se
*/
public class ATMClient {
    private static int connectionPort = 8989;
    static Scanner in = null;
    static int language = 1;
    static String[] newLang;
    static String motd;
    
    public static void main(String[] args) throws IOException {
        Socket ATMSocket = null;
        String adress = "";

        try {
            adress = args[0];
        } catch (ArrayIndexOutOfBoundsException e) {
            System.err.println("Missing argument ip-adress");
            System.exit(1);
        }
        try {
            ATMSocket = new Socket(adress, connectionPort); 
            in = new Scanner(ATMSocket.getInputStream());
        } catch (UnknownHostException e) {
            System.err.println("Unknown host: " +adress);
            System.exit(1);
        } catch (IOException e) {
            System.err.println("Couldn't open connection to " + adress);
            System.exit(1);
        }

        System.out.println("Contacting bank ... ");
        
        //Start the thread that handles the input from the client.
        new InputThread(ATMSocket).start();

        boolean exit = false;
        
        //The main loop of the ATMClient, loops as long as the exit message have not been received.
        while(exit == false) {
        		//Wait for the next code from the server.
        		int line = in.nextInt();
        		//Decode the received code.
        		decode(line, language);
        		//If the exit code 16 have been received, exit.
        		if (line == 16) {
        			exit = true;
        			break;
        		}
        }
        
        in.close();
        ATMSocket.close();
        System.exit(1);
    }
    
    //The decode method, decodes the servers messages.
   	private static void decode(int key, int lang) {
   		//The array containing the standard language for the client.
   		String[] eng = {"Please log in.",
						"Card number:",
						"PIN Code:",
						"Login successful.",
						"Something wrong. Try again.",
						"Unexpected input. Try again.",
						"Welcome to teh Banks private menu!",
						"(1)Balance, (2)Withdrawal, (3)Deposit, (4)Exit, (5)Byt språk till svenska",
						"Account balance is: $", //Need extra balance argument
						"Enter amount to withdraw: ",
						"Enter security code for this withdrawal: ",
						"Withdrawal succeeded. Withdrew: $",//Need extra withdrawed amount argument
						"New account balance after withdrawal is: $", //Need extra balance argument
						"Withdrawal failed. Wrong code.",
						"Enter amount to deposit: ",
						"New account balance after deposit is: $", //Need extra balance argument
						"Good bye!",
						"Not a valid choice. Try again."};
		
		//Choses which language to use depending on the language setting.				
		String[] currentlang = null;
   		if (lang == 1) {
   			currentlang = eng;
   		}
   		else {
   			currentlang = newLang;
   		}
		
		switch (key) {
			//If the code to decode is 8, 11, 12 or 15, there was one line of extra data.
			case 8:
			case 11:
			case 12:
			case 15:
				//Get this data by reading the next int and print the corresponding message and data.
				System.out.println(currentlang[key] + receive());
				break;
			//If the code to decode is 20, the server sent the MOTD as an extra line of data. 
			case 20:
				in.nextLine(); //Remove empty line, ???
				//Update the clients motd with the new MOTD.
				motd = in.nextLine();
				break;
			//If the code to decode is 21, the server wants the client to print the MOTD.
			case 21:
				System.out.println(motd);
				break;
			//If the code to decode is 30, the server sent a new language (requested by client).
			case 30:
				//Switch so the decode now uses the new language in future decoding.
				language = language*(-1);
				//Receive the new language with the getLang() method.
				newLang = getLang();
				System.out.println("Language switched.");
				break;
			//All other codes sent by the server corresponds to indexes in the language-array.
			//If received, print the corresponding message.
			default:
				System.out.println(currentlang[key]);
		}
   	}
   	
   	//The method to receive additional data from the server.
   	static String receive() {
   		in.nextLine(); //Remove empty line, ???
   		String received = in.nextLine();
   		//If the received data is a single %.
   		if (received.equals("%")) {
			String out = "";
			String part = in.nextLine();
			//Build the output String from the parts untill a single % is found, then return.
			while (part.equals("%") == false) {
				out += part;
				part = in.nextLine();
			}
			return out;
		}
		//If the data was not a single %, return the data.
		else {
			return received;
		}
   	}
   	
   	//Method that receives a new language from the server.
   	static String[] getLang() {
   		//Language-packs are stored in a (hardcoded) String array. 
   		String[] langToReturn = new String[18];
   		in.nextLine(); //Remove empty line, ???
   		//Each of the entries are sent as a new line. Get all (18 hardcoded) of them.
		for (int i = 0; i < langToReturn.length; i++) {
			langToReturn[i] = in.nextLine();
		}
		return langToReturn;
   	}
    
    //The thread that handles input from the client.
   	static class InputThread extends Thread {
    	Scanner in = null;
    	Socket socket = null;
    	PrintWriter out = null;
    	
    	public InputThread(Socket socket) {
    		super("InputThread");
    		this.socket = socket;
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
    	
    	public void run() {
    		in = new Scanner(System.in);
    		try {
    			out = new PrintWriter(socket.getOutputStream(), true);
    		}catch(IOException e) {
    			e.printStackTrace();
    		}
    		//Always listen for integers to send. (All communications is in integers.)
    		while(true) {
    			//Wait for input and send it to the server. The server knows what input to expect so
    			//no matching of the servers state is required.
				send(in.nextLine());
    		}
    	}
    }
}   
