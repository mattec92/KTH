package se.kth.ict.id2203.components.paxos;

import se.kth.ict.id2203.ports.beb.BebDeliver;
import se.sics.kompics.address.Address;

public class PrepareMessage extends BebDeliver
{
	private static final long serialVersionUID = -8317023721352906352L;

	private int ts;
	private int t;


	public PrepareMessage(Address source, int ts, int t)
	{
		super(source);
		this.ts = ts;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ", ts: " + ts + " t: " + t;
	}

}
