package se.kth.ict.id2203.components.multipaxos;

import java.util.ArrayList;

import se.kth.ict.id2203.ports.fpl.FplDeliver;
import se.sics.kompics.address.Address;

public class PrepareAckMessage extends FplDeliver
{

	private static final long serialVersionUID = -374423557948437340L;

	private int pts;
	private int ts;
	private ArrayList<Object> vsuf;
	private int l;
	private int t;


	protected PrepareAckMessage(Address source, int pts, int ts, ArrayList<Object> vsuf, int l, int t)
	{
		super(source);
		this.pts = pts;
		this.ts = ts;
		this.vsuf = vsuf;
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


	public int getTs()
	{
		return ts;
	}


	public void setTs(int ts)
	{
		this.ts = ts;
	}


	public ArrayList<Object> getVsuf()
	{
		return vsuf;
	}


	public void setVsuf(ArrayList<Object> vsuf)
	{
		this.vsuf = vsuf;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ". pts: " + pts + ", ts: " + ts + ", vsuf: " + vsuf.toString() + ", l: " + l + ", t: " + t;
	}

}
