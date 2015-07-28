package se.kth.ict.id2203.ports.rb;

import se.sics.kompics.PortType;

public class ReliableBroadcast extends PortType {
	{
		indication(RbDeliver.class);
		request(RbBroadcast.class);
	}
}
