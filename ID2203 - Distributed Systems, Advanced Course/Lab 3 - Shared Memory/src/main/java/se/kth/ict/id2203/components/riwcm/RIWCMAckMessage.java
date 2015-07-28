package se.kth.ict.id2203.components.riwcm;

import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class RIWCMAckMessage extends Pp2pDeliver
{

	private static final long serialVersionUID = 5366893202186924340L;

	private int r;


	protected RIWCMAckMessage(Address source, int r)
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
