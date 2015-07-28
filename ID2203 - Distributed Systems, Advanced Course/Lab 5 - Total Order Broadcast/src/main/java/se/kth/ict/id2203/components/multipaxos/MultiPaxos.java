package se.kth.ict.id2203.components.multipaxos;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.ict.id2203.ports.asc.AbortableSequenceConsensus;
import se.kth.ict.id2203.ports.asc.AscAbort;
import se.kth.ict.id2203.ports.asc.AscDecide;
import se.kth.ict.id2203.ports.asc.AscPropose;
import se.kth.ict.id2203.ports.fpl.FIFOPerfectPointToPointLink;
import se.kth.ict.id2203.ports.fpl.FplSend;
import se.sics.kompics.ComponentDefinition;
import se.sics.kompics.Handler;
import se.sics.kompics.Negative;
import se.sics.kompics.Positive;
import se.sics.kompics.address.Address;

public class MultiPaxos extends ComponentDefinition
{

	private static final Logger logger = LoggerFactory.getLogger(MultiPaxos.class);

	private Negative<AbortableSequenceConsensus> asc = provides(AbortableSequenceConsensus.class);
	private Positive<FIFOPerfectPointToPointLink> fpl = requires(FIFOPerfectPointToPointLink.class);

	private Address selfAddress;
	private ArrayList<Address> allProcesses = new ArrayList<Address>();

	int t = 0;
	int prepts = 0;

	int ats = 0;
	ArrayList<Object> av = new ArrayList<Object>();
	int al = 0;

	int pts = 0;
	ArrayList<Object> pv = new ArrayList<Object>();
	int pl = 0;

	ArrayList<Object> proposedValues = new ArrayList<Object>();

	Map<Address, Holder> readlist = new HashMap<Address, Holder>();
	Map<Address, Integer> accepted = new HashMap<Address, Integer>();
	Map<Address, Integer> decided = new HashMap<Address, Integer>();


	public MultiPaxos(MultiPaxosInit event)
	{
		logger.info("Constructing MultiPaxos component.");

		selfAddress = event.getSelfAddress();
		allProcesses.addAll(event.getAllAddresses());

		for (Address address : allProcesses)
		{
			accepted.put(address, 0);
			decided.put(address, 0);
		}

		subscribe(proposeHandler, asc);
		subscribe(prepareHandler, fpl);
		subscribe(nackHandler, fpl);
		subscribe(prepareAckHandler, fpl);
		subscribe(acceptHandler, fpl);
		subscribe(acceptAckHandler, fpl);
		subscribe(decideHandler, fpl);
	}

	Handler<AscPropose> proposeHandler = new Handler<AscPropose>()
	{

		@Override
		public void handle(AscPropose event)
		{
			logger.debug(event.toString());

			t++;

			if (pts == 0)
			{
				pts = t * allProcesses.size() + selfAddress.getId();
				pv = prefix(av, al);
				pl = 0;

				proposedValues.clear();
				proposedValues.add(event.getValue());

				readlist.clear();

				for (Address address : allProcesses)
				{
					accepted.put(address, 0);
					decided.put(address, 0);
				}

				for (Address address : allProcesses)
				{
					PrepareMessage deliverEvent = new PrepareMessage(selfAddress, pts, al, t);
					FplSend sendEvent = new FplSend(address, deliverEvent);
					trigger(sendEvent, fpl);
				}
			}
			else if (readlist.size() <= (allProcesses.size() / 2))
			{
				proposedValues.add(event.getValue());
			}
			else if (!pv.contains(event.getValue()))
			{
				pv.add(event.getValue());

				for (Address address : allProcesses)
				{
					if (readlist.containsKey(address))
					{
						ArrayList<Object> listOfV = new ArrayList<Object>();
						listOfV.add(event.getValue());

						AcceptMessage deliverEvent = new AcceptMessage(selfAddress, pts, listOfV, pv.size() - 1, t);
						FplSend sendEvent = new FplSend(address, deliverEvent);
						trigger(sendEvent, fpl);
					}
				}
			}
		}
	};

	Handler<PrepareMessage> prepareHandler = new Handler<PrepareMessage>()
	{

		@Override
		public void handle(PrepareMessage event)
		{
			logger.debug(event.toString());

			t = Math.max(t, event.getT()) + 1;

			if (event.getTs() < prepts)
			{
				NackMessage deliverEvent = new NackMessage(selfAddress, event.getTs(), t);
				FplSend sendEvent = new FplSend(event.getSource(), deliverEvent);
				trigger(sendEvent, fpl);
				// logger.debug("Sent " + deliverEvent.toString());
			}
			else
			{
				prepts = event.getTs();

				PrepareAckMessage deliverEvent = new PrepareAckMessage(selfAddress, event.getTs(), ats, suffix(av, event.getL()), al, t);
				FplSend sendEvent = new FplSend(event.getSource(), deliverEvent);
				trigger(sendEvent, fpl);
				// logger.debug("Sent " + deliverEvent.toString());
			}
		}
	};

	Handler<NackMessage> nackHandler = new Handler<NackMessage>()
	{

		@Override
		public void handle(NackMessage event)
		{
			t = Math.max(t, event.getT()) + 1;

			if (event.getPts() == pts)
			{
				pts = 0;
				trigger(new AscAbort(), asc);
			}
		}
	};

	Handler<PrepareAckMessage> prepareAckHandler = new Handler<PrepareAckMessage>()
	{

		@Override
		public void handle(PrepareAckMessage event)
		{
			logger.debug(event.toString());

			t = Math.max(t, event.getT()) + 1;

			if (event.getPts() == pts)
			{
				readlist.put(event.getSource(), new Holder(event.getTs(), event.getVsuf()));
				decided.put(event.getSource(), event.getL());

				if (readlist.size() == ((allProcesses.size() / 2) + 1))
				{
					Holder selectedHolder = new Holder(0, new ArrayList<Object>());
					for (Holder holder : readlist.values())
					{
						if (selectedHolder.ts < holder.ts || (selectedHolder.ts == holder.ts && selectedHolder.vsuf.size() < holder.vsuf.size()))
						{
							selectedHolder = holder;
						}
					}

					pv.addAll(selectedHolder.vsuf);

					for (Object object : proposedValues)
					{
						if (!pv.contains(object))
						{
							pv.add(object);
						}
					}

					for (Address address : allProcesses)
					{
						if (readlist.containsKey(address))
						{
							int lprim = decided.get(address);
							AcceptMessage deliverEvent = new AcceptMessage(selfAddress, pts, suffix(pv, lprim), lprim, t);
							FplSend sendEvent = new FplSend(address, deliverEvent);
							trigger(sendEvent, fpl);
						}
					}
				}
				else if (readlist.size() > ((allProcesses.size() / 2) + 1))
				{
					AcceptMessage deliverEvent = new AcceptMessage(selfAddress, pts, suffix(pv, event.getL()), event.getL(), t);
					FplSend sendEvent = new FplSend(event.getSource(), deliverEvent);
					trigger(sendEvent, fpl);

					if (pl != 0)
					{
						DecideMessage decideDeliverEvent = new DecideMessage(selfAddress, pts, pl, t);
						FplSend decideSendEvent = new FplSend(event.getSource(), decideDeliverEvent);
						trigger(decideSendEvent, fpl);
					}
				}
			}
		}
	};

	Handler<AcceptMessage> acceptHandler = new Handler<AcceptMessage>()
	{

		@Override
		public void handle(AcceptMessage event)
		{
			logger.debug(event.toString());

			t = Math.max(t, event.getT()) + 1;

			if (event.getTs() != prepts)
			{
				NackMessage deliverEvent = new NackMessage(selfAddress, event.getTs(), t);
				FplSend sendEvent = new FplSend(event.getSource(), deliverEvent);
				trigger(sendEvent, fpl);
			}
			else
			{
				ats = event.getTs();

				if (event.getOffs() < av.size())
				{
					av = prefix(av, event.getOffs());
				}

				av.addAll(event.getVsuf());

				AcceptAckMessage deliverEvent = new AcceptAckMessage(selfAddress, event.getTs(), av.size(), t);
				FplSend sendEvent = new FplSend(event.getSource(), deliverEvent);
				trigger(sendEvent, fpl);
			}
		}
	};

	Handler<AcceptAckMessage> acceptAckHandler = new Handler<AcceptAckMessage>()
	{

		@Override
		public void handle(AcceptAckMessage event)
		{
			logger.debug(event.toString());

			t = Math.max(t, event.getT()) + 1;

			if (event.getPts() == pts)
			{
				accepted.put(event.getSource(), event.getL());

				int acceptedProcessesCount = 0;
				for (Address address : allProcesses)
				{
					if (accepted.get(address) >= event.getL())
					{
						acceptedProcessesCount++;
					}
				}

				if (pl < event.getL() && acceptedProcessesCount > (allProcesses.size() / 2))
				{
					pl = event.getL();

					for (Address address2 : allProcesses)
					{
						if (readlist.containsKey(address2))
						{
							DecideMessage deliverEvent = new DecideMessage(selfAddress, pts, pl, t);
							FplSend sendEvent = new FplSend(address2, deliverEvent);
							trigger(sendEvent, fpl);
						}
					}
				}
			}
		}
	};

	Handler<DecideMessage> decideHandler = new Handler<DecideMessage>()
	{

		@Override
		public void handle(DecideMessage event)
		{
			logger.debug(event.toString());

			t = Math.max(t, event.getT()) + 1;

			if (event.getTs() == prepts)
			{
				while (al < event.getL())
				{
					trigger(new AscDecide(av.get(al)), asc);
					al++;
				}
			}
		}
	};

	private static class Holder
	{
		int ts;
		ArrayList<Object> vsuf;


		public Holder(int ts, ArrayList<Object> vsuf)
		{
			this.ts = ts;
			this.vsuf = vsuf;
		}
	}


	private ArrayList<Object> prefix(ArrayList<Object> values, int length)
	{
		ArrayList<Object> prefix = new ArrayList<Object>(values.subList(0, Math.min(length, Math.max(0, values.size() - 1))));
		logger.debug("Prefix of " + values.toString() + " of length " + length + " is " + prefix.toString());
		return prefix;
	}


	private ArrayList<Object> suffix(ArrayList<Object> values, int length)
	{
		if (length > values.size())
		{
			return new ArrayList<Object>();
		}
		else
		{
			ArrayList<Object> suffix = new ArrayList<Object>(values.subList(length, values.size()));
			logger.debug("Suffix of " + values.toString() + " of length " + length + " is " + suffix.toString());
			return suffix;
		}
	}

}
