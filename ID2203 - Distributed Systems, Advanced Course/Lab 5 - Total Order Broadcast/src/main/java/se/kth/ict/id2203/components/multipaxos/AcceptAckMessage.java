package se.kth.ict.id2203.components.multipaxos;

import se.kth.ict.id2203.ports.fpl.FplDeliver;
import se.sics.kompics.address.Address;

public class AcceptAckMessage extends FplDeliver
{

	private static final long serialVersionUID = 2127201910987197066L;

	private int pts;
	private int l;
	private int t;


	protected AcceptAckMessage(Address source, int pts, int l, int t)
	{
		super(source);
		this.pts = pts;
		this.l = l;
		this.t = t;
	}


	public int getPts()
	{
		return pts;
	}


	public void setPts(int pts)
	{
		this.pts = pts;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ". pts: " + pts + ", l: " + l + ", t: " + t;
	}

}
