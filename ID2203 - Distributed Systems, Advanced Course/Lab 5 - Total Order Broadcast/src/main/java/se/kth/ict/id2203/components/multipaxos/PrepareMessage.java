package se.kth.ict.id2203.components.multipaxos;

import se.kth.ict.id2203.ports.fpl.FplDeliver;
import se.sics.kompics.address.Address;

public class PrepareMessage extends FplDeliver
{

	private static final long serialVersionUID = 1172872392613517673L;

	private int ts;
	private int l;
	private int t;


	protected PrepareMessage(Address source, int ts, int l, int t)
	{
		super(source);
		this.ts = ts;
		this.l = l;
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


	public int getL()
	{
		return l;
	}


	public void setL(int l)
	{
		this.l = l;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ". ts: " + ts + ", l: " + l + ", t: " + t;
	}

}
