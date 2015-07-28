package se.kth.ict.id2203.components.riwcm;

import se.kth.ict.id2203.ports.beb.BebDeliver;
import se.sics.kompics.address.Address;

public class RIWCMReadMessage extends BebDeliver
{

	private static final long serialVersionUID = 5723067792648519580L;

	private int r;


	public RIWCMReadMessage(Address source, int r)
	{
		super(source);
		this.r = r;
	}


	public int getR()
	{
		return r;
	}


	public void setR(int r)
	{
		this.r = r;
	}

}
