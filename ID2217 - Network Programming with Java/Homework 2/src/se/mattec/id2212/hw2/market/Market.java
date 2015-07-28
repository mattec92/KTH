package se.mattec.id2212.hw2.market;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

public interface Market extends Remote 
{

	public void putOffer(String name, int price, OfferTypeEnum type, OfferCallback callback) throws RemoteException;
	
	public void removeOffer(Offer offer) throws RemoteException;
	
	public List<Offer> listOffers() throws RemoteException;
	
	public void acceptOffer(Offer offer) throws RemoteException;
	
}
