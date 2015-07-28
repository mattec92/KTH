package se.mattec.id2212.hw2.market;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Transient;

@Entity
public class OfferImpl extends UnicastRemoteObject implements Offer
{
	@Id
	private String key;
	private String name;
	private int price;
	private OfferTypeEnum type;
	private String userId;
	private int amount;
	@Transient
	private OfferCallback callback;
	
	public OfferImpl() throws RemoteException {
		super();
	}


	/**
	 * Constructor, just initiating the Offer.
	 * */
	public OfferImpl(String name, int price, OfferTypeEnum type, OfferCallback callback, String userId, int amount)
			throws RemoteException
	{
		super();
		this.name = name;
		this.price = price;
		this.type = type;
		this.callback = callback;
		this.userId = userId;
		this.amount = amount;
		this.key = getKey();
	}


	/**
	 * Builds the key for this offer. Consists of name + price + type.
	 * */
	@Override
	public String getKey() throws RemoteException
	{
		return name + String.valueOf(price) + type.toString();
	}


	/**
	 * Returns the price of the offer.
	 * */
	@Override
	public int getPrice() throws RemoteException
	{
		return price;
	}


	/**
	 * Returns the name of the offer.
	 * */
	@Override
	public String getName() throws RemoteException
	{
		return name;
	}


	/**
	 * Returns the type of the offer. (Buy/Sell offer)
	 * */
	@Override
	public OfferTypeEnum getType() throws RemoteException
	{
		return type;
	}


	/**
	 * Returns the callback registered with the offer. Reference to the client.
	 * */
	@Override
	public OfferCallback getCallback() throws RemoteException
	{
		return callback;
	}


	/**
	 * Returns the id of the user who created the offer. The owner.
	 * */
	@Override
	public String getuserId() throws RemoteException
	{
		return userId;
	}


	/**
	 * Returns the amount of the offer.
	 * */
	@Override
	public int getAmount() throws RemoteException
	{
		return amount;
	}


	/**
	 * Used when an offer is accepted. Sends a notification to the owner of the
	 * offer.
	 * */
	@Override
	public void acceptOffer() throws RemoteException
	{
		if (callback != null)
		{
			callback.notify(this, null, "Your offer was accepted.");
		}
	}


	/**
	 * User when two matching offers are found. Sends a notification to the
	 * owner of the offer.
	 * */
	@Override
	public void notifyMatchingOfferAvailable(Offer matchingOffer) throws RemoteException
	{
		if (callback != null)
		{
			callback.notify(this, matchingOffer, "An offer matching your offer is available.");
		}
	}


	/**
	 * Checks if two offers are matching. Matching if names are equal, one offer
	 * is a buy and the other a sell offer and the sell price is lower or equal
	 * to the sell price.
	 * */
	@Override
	public boolean isOfferMatching(Offer offer) throws RemoteException
	{
		if (offer.getName().equals(name)
				&& (offer.getType() == OfferTypeEnum.BUY_OFFER && type == OfferTypeEnum.SELL_OFFER
						&& offer.getPrice() >= price || offer.getType() == OfferTypeEnum.SELL_OFFER
						&& type == OfferTypeEnum.BUY_OFFER && offer.getPrice() <= price))
		{
			return true;
		}

		return false;
	}


	/**
	 * Returns a string representation of the offer.
	 * */
	public String getStringRepresentation()
	{
		return String.format("Owner: %s, %s, %s, price: %d, amount: %d", userId, type.toString(), name, price, amount);
	}

}
