package se.kth.ict.id2203.components.paxos;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.ict.id2203.ports.ac.AbortableConsensus;
import se.kth.ict.id2203.ports.ac.AcAbort;
import se.kth.ict.id2203.ports.ac.AcDecide;
import se.kth.ict.id2203.ports.ac.AcPropose;
import se.kth.ict.id2203.ports.beb.BebBroadcast;
import se.kth.ict.id2203.ports.beb.BestEffortBroadcast;
import se.kth.ict.id2203.ports.pp2p.PerfectPointToPointLink;
import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.kth.ict.id2203.ports.pp2p.Pp2pSend;
import se.sics.kompics.ComponentDefinition;
import se.sics.kompics.Handler;
import se.sics.kompics.Negative;
import se.sics.kompics.Positive;
import se.sics.kompics.address.Address;

public class Paxos extends ComponentDefinition
{

	private static final Logger logger = LoggerFactory.getLogger(Paxos.class);

	private Negative<AbortableConsensus> ac = provides(AbortableConsensus.class);
	private Positive<BestEffortBroadcast> beb = requires(BestEffortBroadcast.class);
	private Positive<PerfectPointToPointLink> pp2p = requires(PerfectPointToPointLink.class);

	int t = 0; // timestamp
	int prepts = 0; // preparedTimestamp

	int ats = 0; // acceptedTimestamp
	int av; // acceptedValue

	int pts = 0; // proposerTimestamp
	int pv; // proposedValue

	Map<Address, Holder> readlist = new HashMap<Address, Holder>();

	int acks = 0;

	private Address selfAddress;
	private List<Address> allProcesses = new ArrayList<Address>();


	public Paxos(PaxosInit init)
	{
		selfAddress = init.getSelfAddress();
		allProcesses.addAll(init.getAllAddresses());
		
		subscribe(proposeHandler, ac);
		subscribe(prepareHandler, beb);
		subscribe(nackHandler, pp2p);
		subscribe(prepareAckHandler, pp2p);
		subscribe(acceptHandler, beb);
		subscribe(acceptAckHandler, pp2p);
	}

	Handler<AcPropose> proposeHandler = new Handler<AcPropose>()
	{

		@Override
		public void handle(AcPropose event)
		{
//			logger.debug(event.toString());
			
			t++;
			pts = t * allProcesses.size() + selfAddress.getId();
			pv = event.getValue();
			readlist.clear();
			acks = 0;
			trigger(new BebBroadcast(new PrepareMessage(selfAddress, pts, t)), beb);
		}
	};

	Handler<PrepareMessage> prepareHandler = new Handler<PrepareMessage>()
	{

		@Override
		public void handle(PrepareMessage event)
		{
//			logger.debug(event.toString());
			
			t = Math.max(t, event.getT()) + 1;
			if (event.getTs() < prepts)
			{
				Pp2pDeliver deliverEvent = new NackMessage(selfAddress, event.getTs(), t);
				Pp2pSend sendEvent = new Pp2pSend(event.getSource(), deliverEvent);
				trigger(sendEvent, pp2p);
			}
			else
			{
				prepts = event.getTs();
				
				Pp2pDeliver deliverEvent = new PrepareAckMessage(selfAddress, ats, av, event.getTs(), t);
				Pp2pSend sendEvent = new Pp2pSend(event.getSource(), deliverEvent);
				trigger(sendEvent, pp2p);
			}
		}
	};

	Handler<NackMessage> nackHandler = new Handler<NackMessage>()
	{

		@Override
		public void handle(NackMessage event)
		{
//			logger.debug(event.toString());
			
			t = Math.max(t, event.getT()) + 1;
			if (event.getPts() == pts)
			{
				pts = 0;
				trigger(new AcAbort(), ac);
			}
		}
	};

	Handler<PrepareAckMessage> prepareAckHandler = new Handler<PrepareAckMessage>()
	{

		@Override
		public void handle(PrepareAckMessage event)
		{
//			logger.debug(event.toString());
			
			t = Math.max(t, event.getT()) + 1;
			if (event.getPts() == pts)
			{
				readlist.put(event.getSource(), new Holder(event.getTs(), event.getV()));
				if (readlist.size() > (allProcesses.size() / 2))
				{
					Holder maxHolder = new Holder(Integer.MIN_VALUE, Integer.MIN_VALUE);
					for (Holder holder : readlist.values())
					{
						if (holder.t > maxHolder.t)
						{
							maxHolder = holder;
						}
					}

					if (maxHolder.t != 0)
					{
						pv = maxHolder.v;
					}

					readlist.clear();

					trigger(new BebBroadcast(new AcceptMessage(selfAddress, pts, pv, t)), beb);
				}
			}
		}
	};

	Handler<AcceptMessage> acceptHandler = new Handler<AcceptMessage>()
	{

		@Override
		public void handle(AcceptMessage event)
		{
//			logger.debug(event.toString());
			
			t = Math.max(t, event.getT()) + 1;
			if (event.getTs() < prepts)
			{
				Pp2pDeliver deliverEvent = new NackMessage(selfAddress, event.getTs(), t);
				Pp2pSend sendEvent = new Pp2pSend(event.getSource(), deliverEvent);
				trigger(sendEvent, pp2p);
			}
			else
			{
				ats = prepts = event.getTs();
				av = event.getV();

				Pp2pDeliver deliverEvent = new AcceptAckMessage(selfAddress, event.getTs(), t);
				Pp2pSend sendEvent = new Pp2pSend(event.getSource(), deliverEvent);
				trigger(sendEvent, pp2p);
			}
		}
	};

	Handler<AcceptAckMessage> acceptAckHandler = new Handler<AcceptAckMessage>()
	{

		@Override
		public void handle(AcceptAckMessage event)
		{
//			logger.debug(event.toString());
			
			t = Math.max(t, event.getT()) + 1;
			if (event.getPts() == pts)
			{
				acks++;
				if (acks > (allProcesses.size() / 2))
				{
					pts = 0;
					trigger(new AcDecide(pv), ac);
				}
			}
		}
	};

	private static class Holder
	{
		int t;
		int v;


		public Holder(int t, int v)
		{
			this.t = t;
			this.v = v;
		}
	}

}
