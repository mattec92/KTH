package se.mattec.id2212.hw2.market;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

import se.mattec.id2212.hw2.bank.Account;
import se.mattec.id2212.hw2.bank.Bank;
import se.mattec.id2212.hw2.bank.RejectedException;

public class MarketImpl extends UnicastRemoteObject implements Market
{
	private HashMap<String, Offer> offers;
	private HashMap<String, User> users;

	private Bank bank;

	private EntityManagerFactory emf;
	private EntityManager em;


	/**
	 * 
	 * Constructor. Initiates the HashMap holding the offers.
	 * */
	public MarketImpl(Bank bank) throws RemoteException
	{
		super();
		this.bank = bank;

		this.offers = new HashMap<String, Offer>();
		this.users = new HashMap<String, User>();

		this.emf = Persistence.createEntityManagerFactory("$objectdb/db/marketdb.odb");
		this.em = emf.createEntityManager();

		Query offersQuery = em.createQuery("SELECT o FROM OfferImpl o", OfferImpl.class);
		List<Offer> offersFromDB = offersQuery.getResultList();
		for (Offer offer : offersFromDB)
		{
			offers.put(offer.getKey(), offer);
		}

		Query usersQuery = em.createQuery("SELECT u FROM User u", User.class);
		List<User> usersFromDB = usersQuery.getResultList();
		for (User user : usersFromDB)
		{
			users.put(user.getUsername(), user);
		}
	}


	/**
	 * Adds an offer to the collection. Checks if there exists a matching offer.
	 * If it does, it will send notifications to the clients owning the offers.
	 * */
	@Override
	public void putOffer(String name, int price, OfferTypeEnum type, OfferCallback callback, String userId, int amount)
			throws RemoteException, RejectedException
	{
		if (name == null || name.isEmpty() || price <= 0)
		{
			throw new RejectedException("Name and price is required (price needs to be positive)");
		}

		OfferImpl offerToAdd = new OfferImpl(name, price, type, callback, userId, amount);

		// No duplicate offers are allowed.
		if (offers.containsKey(offerToAdd.getKey()))
		{
			throw new RejectedException("Duplicate offer exists.");
		}

		// Find offers that match the offer being added. Accepting right away.
		for (Offer offer : offers.values())
		{
			if (offer.isOfferMatching(offerToAdd))
			{
				offerToAdd.notifyMatchingOfferAvailable(offer);
				offer.notifyMatchingOfferAvailable(offerToAdd);

				break;
			}
		}

		// Add to HashMap + persist to DB.
		offers.put(offerToAdd.getKey(), offerToAdd);
		em.getTransaction().begin();
		em.persist(offerToAdd);
		em.getTransaction().commit();
	}


	/**
	 * Removes specified offer from the collection.
	 * */
	@Override
	public void removeOffer(Offer offer) throws RemoteException, RejectedException
	{
		if (!offers.containsKey(offer.getKey()))
		{
			throw new RejectedException("No such offer exists.");
		}

		// Remove from HashMap + DB.
		em.getTransaction().begin();
		Offer localOffer = offers.get(offer.getKey());
		offers.remove(offer.getKey());
		Offer managed = em.merge(localOffer);
		em.remove(managed);
		em.getTransaction().commit();
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
	public void acceptOffer(Offer offer, String userId) throws RemoteException, RejectedException
	{
		if (!offers.containsKey(offer.getKey()))
		{
			throw new IllegalStateException("No such offer exists.");
		}

		/*
		 * If this is a sell offer, the one who accepts the offer need funds.
		 * Funds are checked by using the rejected exception in the bank.
		 */
		Account offerOwnersAccount = bank.getAccount(offer.getuserId());
		Account acceptersAccount = bank.getAccount(userId);

		if (offer.getType() == OfferTypeEnum.SELL_OFFER)
		{
			// Accepter need funds
			try
			{
				if (acceptersAccount != null)
				{
					acceptersAccount.withdraw(offer.getPrice());
				}
				if (offerOwnersAccount != null)
				{
					offerOwnersAccount.deposit(offer.getPrice());
				}
			}
			catch (RejectedException e1)
			{
				throw new RejectedException("You dont have enough funds in your bank account.");
			}
		}
		else if (offer.getType() == OfferTypeEnum.BUY_OFFER)
		{
			// Offer owner need funds
			try
			{
				if (offerOwnersAccount != null)
				{
					offerOwnersAccount.withdraw(offer.getPrice());
				}
				if (acceptersAccount != null)
				{
					acceptersAccount.deposit(offer.getPrice());
				}
			}
			catch (RejectedException e1)
			{
				throw new RejectedException("You dont have enough funds in your bank account.");
			}
		}

		// Get the users and increment their buy/sell statistics.
		User offerOwner = users.get(offer.getuserId());
		User offerAccepter = users.get(userId);

		if (offer.getType() == OfferTypeEnum.SELL_OFFER)
		{
			offerOwner.incrementSellCount();
			offerAccepter.incrementBuyCount();
		}
		else
		{
			offerOwner.incrementBuyCount();
			offerAccepter.incrementSellCount();
		}

		em.getTransaction().begin();
		em.persist(offerOwner);
		em.persist(offerAccepter);

		// Remove offer from HashMap + DB.
		Offer localOffer = offers.get(offer.getKey());
		localOffer.acceptOffer();
		offers.remove(offer.getKey());
		Offer managed = em.merge(localOffer);
		em.remove(managed);
		em.getTransaction().commit();
	}


	/**
	 * Used to log in.
	 * */
	@Override
	public boolean logIn(String username, String password) throws RemoteException, RejectedException
	{
		User user = users.get(username);

		if (user != null && user.getUsername().equals(username) && user.getPassword().equals(password))
		{
			tryCreateBankAccount(username);
			return true;
		}

		return false;
	}


	/**
	 * Used to register a new user. Add to hashmap and db.
	 * */
	@Override
	public void register(String username, String password) throws RemoteException, RejectedException
	{
		User user = new User(username, password);

		User matchingUser = em.find(User.class, username);

		if (matchingUser == null)
		{
			if (password.length() >= 8)
			{
				em.getTransaction().begin();
				users.put(username, user);
				em.persist(user);
				em.getTransaction().commit();

				tryCreateBankAccount(username);
			}
			else
			{
				throw new RejectedException("Password too short. Must be at least 8 characters");
			}
		}
		else
		{
			throw new RejectedException("Already registered");
		}
	}


	/**
	 * Tries to create a bank account if none exists for the user.
	 * */
	private void tryCreateBankAccount(String username) throws RemoteException, RejectedException
	{
		Account userAccount = bank.getAccount(username);

		if (userAccount == null)
		{
			try
			{
				bank.newAccount(username);
			}
			catch (RejectedException e)
			{
				e.printStackTrace();
			}
		}
	}


	/**
	 * Returns the number of items the user sold.
	 * */
	@Override
	public int getSellCount(String username) throws RemoteException
	{
		User user = users.get(username);
		if (user != null)
		{
			return user.getSellCount();
		}

		return 0;
	}


	/**
	 * Returns the number of items the user bought.
	 * */
	@Override
	public int getBuyCount(String username) throws RemoteException
	{
		User user = users.get(username);
		if (user != null)
		{
			return user.getBuyCount();
		}

		return 0;
	}

}
