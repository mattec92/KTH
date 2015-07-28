package se.mattec.id2212.hw2.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import javax.persistence.Entity;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Id;
import javax.persistence.Persistence;
import javax.persistence.Transient;

@SuppressWarnings("serial")
@Entity
public class AccountImpl extends UnicastRemoteObject implements Account {
	@Id
    private String name;
    private float balance = 0;
    
    @Transient
	private EntityManagerFactory emf;
    @Transient
	private EntityManager em;
    
    public AccountImpl() throws RemoteException {
    	super();

		this.emf = Persistence.createEntityManagerFactory("$objectdb/db/marketdb.odb");
		this.em = emf.createEntityManager();
    }

    /**
     * Constructs a persistently named object.
     */
    public AccountImpl(String name) throws RemoteException {
        this();
        this.name = name;

		em.getTransaction().begin();
		em.persist(this);
		em.getTransaction().commit();
    }

    @Override
    public synchronized void deposit(float value) throws RemoteException,
                                                         RejectedException {
        if (value < 0) {
            throw new RejectedException("Rejected: Account " + name + ": Illegal value: " + value);
        }
        balance += value;
        System.out.println("Transaction: Account " + name + ": deposit: $" + value + ", balance: $"
                           + balance);

		em.getTransaction().begin();
		Account managed = em.merge(this);
		em.persist(managed);
		em.getTransaction().commit();
    }

    @Override
    public synchronized void withdraw(float value) throws RemoteException,
                                                          RejectedException {
        if (value < 0) {
            throw new RejectedException("Rejected: Account " + name + ": Illegal value: " + value);
        }
        if ((balance - value) < 0) {
            throw new RejectedException("Rejected: Account " + name
                                        + ": Negative balance on withdraw: " + (balance - value));
        }
        balance -= value;
        System.out.println("Transaction: Account " + name + ": withdraw: $" + value + ", balance: $"
                           + balance);

		em.getTransaction().begin();
		Account managed = em.merge(this);
		em.persist(managed);
		em.getTransaction().commit();
    }

    @Override
    public synchronized float getBalance() throws RemoteException {
        return balance;
    }

	@Override
	public String getName() throws RemoteException
	{
		return name;
	}
}
