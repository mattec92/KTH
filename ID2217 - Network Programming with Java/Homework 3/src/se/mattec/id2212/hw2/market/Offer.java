package se.mattec.id2212.hw2.market;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Offer extends Remote
{

	public String getKey() throws RemoteException;


	public int getPrice() throws RemoteException;


	public String getName() throws RemoteException;


	public OfferTypeEnum getType() throws RemoteException;


	public OfferCallback getCallback() throws RemoteException;


	public String getuserId() throws RemoteException;


	public int getAmount() throws RemoteException;


	public void acceptOffer() throws RemoteException;


	public void notifyMatchingOfferAvailable(Offer matchingOffer) throws RemoteException;


	public boolean isOfferMatching(Offer offer) throws RemoteException;


	public String getStringRepresentation() throws RemoteException;

}
