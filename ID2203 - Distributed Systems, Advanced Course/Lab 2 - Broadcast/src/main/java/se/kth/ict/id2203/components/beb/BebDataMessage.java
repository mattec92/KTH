package se.kth.ict.id2203.components.beb;

import se.kth.ict.id2203.ports.beb.BebDeliver;
import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class BebDataMessage extends Pp2pDeliver
{
	private BebDeliver deliverEvent;

	private static final long serialVersionUID = 4875200623380249528L;

	protected BebDataMessage(Address source, BebDeliver deliverEvent)
	{
		super(source);
		this.deliverEvent = deliverEvent;
	}

	public BebDeliver getDeliverEvent()
	{
		return deliverEvent;
	}

	public void setDeliverEvent(BebDeliver deliverEvent)
	{
		this.deliverEvent = deliverEvent;
	}

}
