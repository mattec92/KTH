package se.kth.ict.id2203.components.paxos;

import se.kth.ict.id2203.ports.beb.BebDeliver;
import se.sics.kompics.address.Address;

public class AcceptMessage extends BebDeliver
{
	private static final long serialVersionUID = -6111078998281160585L;

	private int ts;
	private int v;
	private int t;


	public AcceptMessage(Address source, int ts, int v, int t)
	{
		super(source);
		this.ts = ts;
		this.v = v;
		this.t = t;
	}


	public int getTs()
	{
		return ts;
	}


	public void setTs(int ts)
	{
		this.ts = ts;
	}


	public int getV()
	{
		return v;
	}


	public void setV(int v)
	{
		this.v = v;
	}


	public int getT()
	{
		return t;
	}


	public void setT(int t)
	{
		this.t = t;
	}


	@Override
	public String toString()
	{
		return getClass().getSimpleName() + " from process " + getSource().getId() + ", ts: " + ts + " v: " + v + " t: " + t;
	}

}
