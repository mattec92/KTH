package se.kth.ict.id2203.components.epfd;

import java.io.Serializable;

import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class HeartbeatReplyMessage extends Pp2pDeliver implements Serializable
{

	private int sequenceNumber;

	/**
	 * 
	 */
	private static final long serialVersionUID = 6394803947171050114L;

	protected HeartbeatReplyMessage(Address source, int sequenceNumber)
	{
		super(source);
		this.sequenceNumber = sequenceNumber;
	}

	public int getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(int sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

}
