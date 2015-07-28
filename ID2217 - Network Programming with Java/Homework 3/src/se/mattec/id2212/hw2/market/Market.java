package se.mattec.id2212.hw2.market;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

import se.mattec.id2212.hw2.bank.RejectedException;

public interface Market extends Remote
{

	public void putOffer(String name, int price, OfferTypeEnum type, OfferCallback callback, String userId, int amount)
			throws RemoteException, RejectedException;


	public void removeOffer(Offer offer) throws RemoteException, RejectedException;


	public List<Offer> listOffers() throws RemoteException;


	public void acceptOffer(Offer offer, String userId) throws RemoteException, RejectedException;


	public boolean logIn(String username, String password) throws RemoteException, RejectedException;


	public void register(String username, String password) throws RemoteException, RejectedException;


	public int getSellCount(String username) throws RemoteException;


	public int getBuyCount(String username) throws RemoteException;
}
