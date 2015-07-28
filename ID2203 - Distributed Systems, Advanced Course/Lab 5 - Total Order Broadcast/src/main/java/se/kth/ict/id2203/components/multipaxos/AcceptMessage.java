package se.kth.ict.id2203.components.multipaxos;

import java.util.ArrayList;

import se.kth.ict.id2203.ports.fpl.FplDeliver;
import se.sics.kompics.address.Address;

public class AcceptMessage extends FplDeliver
{

	private static final long serialVersionUID = -8073417488609069835L;

	private int ts;
	private ArrayList<Object> vsuf;
	private int offs;
	private int t;


	protected AcceptMessage(Address source, int ts, ArrayList<Object> vsuf, int offs, int t)
	{
		super(source);
		this.ts = ts;
		this.vsuf = vsuf;
		this.offs = offs;
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


	public ArrayList<Object> getVsuf()
	{
		return vsuf;
	}


	public void setVsuf(ArrayList<Object> vsuf)
	{
		this.vsuf = vsuf;
	}


	public int getOffs()
	{
		return offs;
	}


	public void setOffs(int offs)
	{
		this.offs = offs;
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
		return getClass().getSimpleName() + " from process " + getSource().getId() + ". ts: " + ts + ", vsuf: " + vsuf.toString() + ", offs: " + offs + ", t: " + t;
	}

}
