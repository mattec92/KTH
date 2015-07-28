package se.kth.ict.id2203.components.rb;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.ict.id2203.ports.beb.BebBroadcast;
import se.kth.ict.id2203.ports.beb.BebDeliver;
import se.kth.ict.id2203.ports.beb.BestEffortBroadcast;
import se.kth.ict.id2203.ports.rb.RbBroadcast;
import se.kth.ict.id2203.ports.rb.ReliableBroadcast;
import se.sics.kompics.ComponentDefinition;
import se.sics.kompics.Handler;
import se.sics.kompics.Negative;
import se.sics.kompics.Positive;
import se.sics.kompics.address.Address;

public class EagerRb extends ComponentDefinition
{

	@SuppressWarnings("unused")
	private static final Logger logger = LoggerFactory.getLogger(EagerRb.class);

	private Positive<BestEffortBroadcast> bebPort = requires(BestEffortBroadcast.class);
	private Negative<ReliableBroadcast> rbPort = provides(ReliableBroadcast.class);

	private Address selfAddress;
	private List<Address> allProcesses = new ArrayList<>();

	private HashSet<RbDataMessage> delivered = new HashSet<RbDataMessage>();
	private int sequenceNumber = 0;


	public EagerRb(EagerRbInit init)
	{
		selfAddress = init.getSelfAddress();
		allProcesses.addAll(init.getAllAddresses());

		subscribe(rbBroadcastHandler, rbPort);
		subscribe(bebDeliverHandler, bebPort);
	}

	Handler<RbBroadcast> rbBroadcastHandler = new Handler<RbBroadcast>()
	{

		@Override
		public void handle(RbBroadcast event)
		{
			// logger.debug("Sending beb broadcast type " +
			// event.getClass().getSimpleName() + " deliver type " +
			// event.getDeliverEvent().getClass().getSimpleName());

			sequenceNumber++;

			BebDeliver deliverEvent = new RbDataMessage(selfAddress, sequenceNumber, event.getDeliverEvent());
			BebBroadcast sendEvent = new BebBroadcast(deliverEvent);
			trigger(sendEvent, bebPort);
		}
	};

	Handler<BebDeliver> bebDeliverHandler = new Handler<BebDeliver>()
	{

		@Override
		public void handle(BebDeliver event)
		{
			// logger.debug("BebDeliver from " + event.getSource() + " type " +
			// event.getClass().getSimpleName());

			if (event instanceof RbDataMessage)
			{
				RbDataMessage rbDataMessage = (RbDataMessage) event;

				if (!delivered.contains(rbDataMessage))
				{
					delivered.add(rbDataMessage);

					trigger(rbDataMessage.getDeliverEvent(), rbPort);

					BebBroadcast sendEvent = new BebBroadcast(rbDataMessage);
					trigger(sendEvent, bebPort);
				}
			}
		}
	};

}
