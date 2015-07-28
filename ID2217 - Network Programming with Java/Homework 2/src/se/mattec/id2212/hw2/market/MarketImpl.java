package se.mattec.id2212.hw2.market;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MarketImpl extends UnicastRemoteObject implements Market
{
	private HashMap<String, Offer> offers;

	
	/**
	 * Constructor. Initiates the HashMap holding the offers.
	 * */
	public MarketImpl() throws RemoteException 
	{
		super();
		this.offers = new HashMap<String, Offer>();
	}

	
	/**
	 * Adds an offer to the collection. Checks if there exists a matching offer.
	 * If it does, it will send notifications to the clients owning the offers.
	 * */
	@Override
	public void putOffer(String name, int price, OfferTypeEnum type, OfferCallback callback) throws RemoteException, IllegalStateException 
	{
		OfferImpl offerToAdd = new OfferImpl(name, price, type, callback);
		
		//No duplicate offers are allowed.
		if (offers.containsKey(offerToAdd.getKey()))
		{
			throw new IllegalStateException("Duplicate offer exists.");
		}

		//Find offers that match the offer being added. Accepting right away.
		for (Offer offer : offers.values())
		{
			if (offer.isOfferMatching(offerToAdd))
			{
				offerToAdd.notifyMatchingOfferAvailable(offer);
				offer.notifyMatchingOfferAvailable(offerToAdd);
				
				break;
			}
		}

		offers.put(offerToAdd.getKey(), offerToAdd);
	}

	
	/**
	 * Removes specified offer from the collection.
	 * */
	@Override
	public void removeOffer(Offer offer) throws RemoteException 
	{
		if (!offers.containsKey(offer.getKey()))
		{
			throw new IllegalStateException("No such offer exists.");
		}

		offers.remove(offer.getKey());
	}
	

	/**
	 * Returns all offers as a list.
	 * */
	@Override
	public List<Offer> listOffers() throws RemoteException 
	{
		return new ArrayList<Offer>(offers.values());
	}
	

	/**
	 * Used to accept an offer. Then removes the offer from the collection.
	 * */
	@Override
	public void acceptOffer(Offer offer) throws RemoteException 
	{
		if (!offers.containsKey(offer.getKey()))
		{
			throw new IllegalStateException("No such offer exists.");
		}

		offers.get(offer.getKey()).acceptOffer();
		offers.remove(offer.getKey());
	}

}
