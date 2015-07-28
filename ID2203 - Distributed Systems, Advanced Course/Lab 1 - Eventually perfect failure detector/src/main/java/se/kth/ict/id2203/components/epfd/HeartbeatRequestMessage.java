package se.kth.ict.id2203.components.epfd;

import java.io.Serializable;

import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.sics.kompics.address.Address;

public class HeartbeatRequestMessage extends Pp2pDeliver implements Serializable
{

	private int sequenceNumber;
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 6688727985149323339L;

	protected HeartbeatRequestMessage(Address source, int sequenceNumber)
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
