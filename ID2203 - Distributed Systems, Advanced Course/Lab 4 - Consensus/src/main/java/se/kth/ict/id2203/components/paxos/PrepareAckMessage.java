package se.kth.ict.id2203.components.paxos;

import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class PrepareAckMessage extends Pp2pDeliver
{
	private static final long serialVersionUID = 3451964138877209840L;

	private int ts;
	private int v;
	private int pts;
	private int t;


	protected PrepareAckMessage(Address source, int ts, int v, int pts, int t)
	{
		super(source);
		this.ts = ts;
		this.v = v;
		this.pts = pts;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ", ts: " + ts + " v: " + v + " pts: " + pts + " t: " + t;
	}

}
