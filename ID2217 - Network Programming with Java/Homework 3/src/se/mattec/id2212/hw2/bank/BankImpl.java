package se.mattec.id2212.hw2.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

import se.mattec.id2212.hw2.market.Offer;
import se.mattec.id2212.hw2.market.OfferImpl;

@SuppressWarnings("serial")
public class BankImpl extends UnicastRemoteObject implements Bank {
    private String bankName;
    private Map<String, Account> accounts = new HashMap<>();

	private EntityManagerFactory emf;
	private EntityManager em;

    public BankImpl(String bankName) throws RemoteException {
        super();
        this.bankName = bankName;

		this.emf = Persistence.createEntityManagerFactory("$objectdb/db/marketdb.odb");
		this.em = emf.createEntityManager();
        
		Query accountsQuery = em.createQuery("SELECT o FROM AccountImpl o", AccountImpl.class);
		List<Account> accountsFromDB = accountsQuery.getResultList();
		for (Account account : accountsFromDB)
		{
			accounts.put(account.getName(), account);
		}
    }

    @Override
    public synchronized String[] listAccounts() {
        return accounts.keySet().toArray(new String[1]);
    }

    @Override
    public synchronized Account newAccount(String name) throws RemoteException,
                                                               RejectedException {
        AccountImpl account = (AccountImpl) accounts.get(name);
        if (account != null) {
            System.out.println("Account [" + name + "] exists!!!");
            throw new RejectedException("Rejected: se.kth.id2212.ex2.Bank: " + bankName
                                        + " Account for: " + name + " already exists: " + account);
        }
        account = new AccountImpl(name);
        accounts.put(name, account);
        System.out.println("se.kth.id2212.ex2.Bank: " + bankName + " Account: " + account
                           + " has been created for " + name);
        return account;
    }

    @Override
    public synchronized Account getAccount(String name) {
        return accounts.get(name);
    }

    @Override
    public synchronized boolean deleteAccount(String name) {
        if (!hasAccount(name)) {
            return false;
        }
        accounts.remove(name);
        System.out.println("se.kth.id2212.ex2.Bank: " + bankName + " Account for " + name
                           + " has been deleted");
        return true;
    }

    private boolean hasAccount(String name) {
        return accounts.get(name) != null;
    }
}
