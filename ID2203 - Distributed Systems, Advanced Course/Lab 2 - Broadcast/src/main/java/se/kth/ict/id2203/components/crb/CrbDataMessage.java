package se.kth.ict.id2203.components.crb;

import java.util.Map;

import se.kth.ict.id2203.ports.crb.CrbDeliver;
import se.kth.ict.id2203.ports.rb.RbDeliver;
import se.sics.kompics.address.Address;

public class CrbDataMessage extends RbDeliver
{

	private Map<Integer, Integer> vector;

	private CrbDeliver deliverEvent;

	private static final long serialVersionUID = 3236160554463192926L;


	public CrbDataMessage(Address source, Map<Integer, Integer> vector, CrbDeliver deliverEvent)
	{
		super(source);
		this.vector = vector;
		this.deliverEvent = deliverEvent;
	}


	public Map<Integer, Integer> getVector()
	{
		return vector;
	}


	public void setVector(Map<Integer, Integer> vector)
	{
		this.vector = vector;
	}


	public CrbDeliver getDeliverEvent()
	{
		return deliverEvent;
	}


	public void setDeliverEvent(CrbDeliver deliverEvent)
	{
		this.deliverEvent = deliverEvent;
	}

}
