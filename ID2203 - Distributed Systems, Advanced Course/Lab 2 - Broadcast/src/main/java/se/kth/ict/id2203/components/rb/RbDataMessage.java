package se.kth.ict.id2203.components.rb;

import se.kth.ict.id2203.ports.beb.BebDeliver;
import se.kth.ict.id2203.ports.rb.RbDeliver;
import se.sics.kompics.address.Address;

public class RbDataMessage extends BebDeliver
{

	private int sequenceNumber;

	private RbDeliver deliverEvent;

	private static final long serialVersionUID = 4875200623380249528L;

	protected RbDataMessage(Address source, int sequenceNumber, RbDeliver deliverEvent)
	{
		super(source);
		this.sequenceNumber = sequenceNumber;
		this.deliverEvent = deliverEvent;
	}

	public int getSequenceNumber()
	{
		return sequenceNumber;
	}

	public void setSequenceNumber(int sequenceNumber)
	{
		this.sequenceNumber = sequenceNumber;
	}

	public RbDeliver getDeliverEvent()
	{
		return deliverEvent;
	}

	public void setDeliverEvent(RbDeliver deliverEvent)
	{
		this.deliverEvent = deliverEvent;
	}

	@Override
	public boolean equals(Object obj)
	{
		return ((RbDataMessage) obj).getSequenceNumber() == sequenceNumber && 
				getSource().getId() == ((RbDataMessage) obj).getSource().getId();
	}

	@Override
	public int hashCode()
	{
		String concatenated = "" + getSource().getId() + "SEPARATOR" + sequenceNumber;
		
		return concatenated.hashCode();
	}

}
