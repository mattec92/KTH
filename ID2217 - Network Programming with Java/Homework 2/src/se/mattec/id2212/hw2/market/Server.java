package se.mattec.id2212.hw2.market;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;

import se.mattec.id2212.hw2.bank.Bank;
import se.mattec.id2212.hw2.bank.BankImpl;

public class Server 
{
	private static final String MARKET_NAME = "Market";
	private static final String BANK_NAME = "Bank";

	
	public static void main(String[] args) 
	{
		new Server();
	}

	
	/**
	 * Constructor. Initiates the market and bank and registers
	 * them with the naming service.
	 * */
	public Server() 
	{
		try 
		{
			Bank bank = new BankImpl(BANK_NAME);
			Market market = new MarketImpl();

			// Register the newly created object at rmiregistry.
			try 
			{
				LocateRegistry.getRegistry(1099).list();
			} 
			catch (RemoteException e) 
			{
				LocateRegistry.createRegistry(1099);
			}

			Naming.rebind(BANK_NAME, bank);
			Naming.rebind(MARKET_NAME, market);
		} 
		catch (Exception e) 
		{
			e.printStackTrace();
		}
	}
	
}
