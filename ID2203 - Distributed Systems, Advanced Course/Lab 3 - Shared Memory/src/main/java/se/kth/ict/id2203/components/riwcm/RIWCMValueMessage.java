package se.kth.ict.id2203.components.riwcm;

import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class RIWCMValueMessage extends Pp2pDeliver
{

	private static final long serialVersionUID = -4049825674476779155L;

	private int r;
	private int ts;
	private int wr;
	private int v;


	protected RIWCMValueMessage(Address source, int r, int ts, int wr, int v)
	{
		super(source);
		this.r = r;
		this.ts = ts;
		this.wr = wr;
		this.v = v;
	}


	public int getR()
	{
		return r;
	}


	public void setR(int r)
	{
		this.r = r;
	}


	public int getTs()
	{
		return ts;
	}


	public void setTs(int ts)
	{
		this.ts = ts;
	}


	public int getWr()
	{
		return wr;
	}


	public void setWr(int wr)
	{
		this.wr = wr;
	}


	public int getV()
	{
		return v;
	}


	public void setV(int v)
	{
		this.v = v;
	}

}
