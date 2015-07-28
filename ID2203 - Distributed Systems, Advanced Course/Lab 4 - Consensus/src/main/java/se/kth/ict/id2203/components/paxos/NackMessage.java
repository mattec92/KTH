package se.kth.ict.id2203.components.paxos;

import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class NackMessage extends Pp2pDeliver
{
	private static final long serialVersionUID = 4411644288897984683L;

	private int pts;
	private int t;


	protected NackMessage(Address source, int pts, int t)
	{
		super(source);
		this.pts = pts;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ", pts: " + pts + " t: " + t;
	}

}
