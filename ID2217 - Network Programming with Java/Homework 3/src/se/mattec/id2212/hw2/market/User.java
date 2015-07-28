package se.mattec.id2212.hw2.market;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
public class User extends UnicastRemoteObject
{
	@Id
	private String username;
	private String password;
	private int buyCount;
	private int sellCount;


	public User(String username, String password) throws RemoteException
	{
		super();
		this.username = username;
		this.password = password;
	}


	public String getUsername()
	{
		return username;
	}


	public void setUsername(String username)
	{
		this.username = username;
	}


	public String getPassword()
	{
		return password;
	}


	public void setPassword(String password)
	{
		this.password = password;
	}


	public int getBuyCount()
	{
		return buyCount;
	}


	public void setBuyCount(int buyCount)
	{
		this.buyCount = buyCount;
	}


	public int getSellCount()
	{
		return sellCount;
	}


	public void setSellCount(int sellCount)
	{
		this.sellCount = sellCount;
	}


	public void incrementSellCount()
	{
		sellCount++;
	}


	public void incrementBuyCount()
	{
		buyCount++;
	}

}
