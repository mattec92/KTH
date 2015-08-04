import java.io.*;   
import java.net.*;  
import java.util.Scanner;

/**
   @author Mattias Cederlund, mcede@kth.se
*/
public class ATMClient {
    private static int connectionPort = 8989;
    
    public static void main(String[] args) throws IOException {
        Socket ATMSocket = null;
        String adress = "";
		Scanner in = null;

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
        
        new InputThread(ATMSocket).start();
        
        boolean exit = false;
        while(exit == false) {
        	while (in.hasNextLine()) {
        		String line = in.nextLine();
        		System.out.println(line);
        		if (line.equals("Good bye!")) {
        			exit = true;
        			break;
        		}
        		if (line.length() == 0) {
        			break;
        		}
        	}
        }
        
        in.close();
        ATMSocket.close();
        System.exit(1);
    }
    
   	static class InputThread extends Thread {
    	Scanner in = null;
    	Socket socket = null;
    	PrintWriter out = null;
    	
    	public InputThread(Socket socket) {
    		super("InputThread");
    		this.socket = socket;
    	}
    	
    	public void run() {
    		in = new Scanner(System.in);
    		try {
    			out = new PrintWriter(socket.getOutputStream(), true);
    		}catch(IOException e) {
    			e.printStackTrace();
    		}
    		while(true) {
    			String toSend = in.nextLine();
    			if (toSend.equals("q")) {
    				break;
    			}
    			out.println(toSend);
    		}
    		System.exit(1);
    	}
    }
}   
