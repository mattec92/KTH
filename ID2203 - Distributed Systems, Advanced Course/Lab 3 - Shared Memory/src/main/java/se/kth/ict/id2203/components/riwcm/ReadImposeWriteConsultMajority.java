package se.kth.ict.id2203.components.riwcm;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.ict.id2203.ports.ar.ArReadRequest;
import se.kth.ict.id2203.ports.ar.ArReadResponse;
import se.kth.ict.id2203.ports.ar.ArWriteRequest;
import se.kth.ict.id2203.ports.ar.ArWriteResponse;
import se.kth.ict.id2203.ports.ar.AtomicRegister;
import se.kth.ict.id2203.ports.beb.BebBroadcast;
import se.kth.ict.id2203.ports.beb.BestEffortBroadcast;
import se.kth.ict.id2203.ports.pp2p.PerfectPointToPointLink;
import se.kth.ict.id2203.ports.pp2p.Pp2pSend;
import se.sics.kompics.ComponentDefinition;
import se.sics.kompics.Handler;
import se.sics.kompics.Negative;
import se.sics.kompics.Positive;
import se.sics.kompics.address.Address;

public class ReadImposeWriteConsultMajority extends ComponentDefinition
{

	private static final Logger logger = LoggerFactory.getLogger(ReadImposeWriteConsultMajority.class);

	private Address selfAddress;
	private Set<Address> allProcesses;

	private Negative<AtomicRegister> nnar = provides(AtomicRegister.class);
	private Positive<BestEffortBroadcast> beb = requires(BestEffortBroadcast.class);
	private Positive<PerfectPointToPointLink> pp2p = requires(PerfectPointToPointLink.class);

	private int ts;
	private int wr;
	private int val;
	private int acks;
	private int writeVal;
	private int rid;
	private Map<Integer, Holder> readList;
	private int readVal;
	private boolean reading;

	public ReadImposeWriteConsultMajority(ReadImposeWriteConsultMajorityInit event)
	{
		this.selfAddress = event.getSelfAddress();
		this.allProcesses = event.getAllAddresses();

		ts = 0;
		wr = 0;
		val = 0;
		acks = 0;
		writeVal = 0; // Undefined
		readList = new HashMap<Integer, Holder>();
		readVal = 0; // Undefined
		reading = false;

		subscribe(readRequestHandler, nnar);
		subscribe(riwcmReadHandler, beb);
		subscribe(pp2pDeliverHandler, pp2p);
		subscribe(writeRequestHandler, nnar);
		subscribe(riwcmWriteHandler, beb);
		subscribe(riwcmAckDeliverHandler, pp2p);
	}

	Handler<ArReadRequest> readRequestHandler = new Handler<ArReadRequest>()
	{

		@Override
		public void handle(ArReadRequest event)
		{
//			logger.debug("ArReadRequest");

			rid += 1;
			acks = 0;
			readList.clear();
			reading = true;

			trigger(new BebBroadcast(new RIWCMReadMessage(selfAddress, rid)), beb);
		}
	};

	Handler<RIWCMReadMessage> riwcmReadHandler = new Handler<RIWCMReadMessage>()
	{

		@Override
		public void handle(RIWCMReadMessage event)
		{
//			logger.debug("RIWCMReadMessage from process" + event.getSource().getId());

			RIWCMValueMessage valueMessage = new RIWCMValueMessage(selfAddress, event.getR(), ts, wr, val);
			Pp2pSend sendEvent = new Pp2pSend(event.getSource(), valueMessage);
			trigger(sendEvent, pp2p);
		}
	};

	Handler<RIWCMValueMessage> pp2pDeliverHandler = new Handler<RIWCMValueMessage>()
	{

		@Override
		public void handle(RIWCMValueMessage event)
		{
			if (event.getR() == rid)
			{
//				logger.debug("RIWCMValueMessage from process" + event.getSource().getId());

				readList.put(event.getSource().getId(), new Holder(event.getTs(), event.getWr(), event.getV()));

				if (readList.size() > (allProcesses.size() / 2))
				{
					Holder maxHolder = new Holder(Integer.MIN_VALUE, Integer.MIN_VALUE, 0);
					for (Holder holder : readList.values())
					{
						if (holder.ts > maxHolder.ts || (holder.ts == maxHolder.ts && holder.wr > maxHolder.wr))
						{
							maxHolder = holder;
						}
					}

					readVal = maxHolder.val;

					readList.clear();

					if (reading)
					{
						trigger(new BebBroadcast(new RIWCMWriteMessage(selfAddress, rid, maxHolder.ts, maxHolder.wr, readVal)), beb);
					} else
					{
						trigger(new BebBroadcast(new RIWCMWriteMessage(selfAddress, rid, maxHolder.ts + 1, selfAddress.getId(), writeVal)), beb);
					}
				}
			} 
//			else
//			{
//				logger.debug("RIWCMValueMessage from process" + event.getSource().getId() + ", R not matching");
//			}
		}
	};

	Handler<ArWriteRequest> writeRequestHandler = new Handler<ArWriteRequest>()
	{

		@Override
		public void handle(ArWriteRequest event)
		{
//			logger.debug("ArWriteReques");

			rid++;
			writeVal = event.getValue();
			acks = 0;
			readList.clear();

			trigger(new BebBroadcast(new RIWCMReadMessage(selfAddress, rid)), beb);
		}
	};

	Handler<RIWCMWriteMessage> riwcmWriteHandler = new Handler<RIWCMWriteMessage>()
	{

		@Override
		public void handle(RIWCMWriteMessage event)
		{
//			logger.debug("RIWCMWriteMessage from process" + event.getSource().getId());

			if (event.getTs() > ts || (event.getTs() == ts && event.getWr() > wr))
			{
//				logger.debug("Will write " + event.getV() + " ts:" + event.getTs() + " oldTs:" + ts + " wr:" + event.getWr() + " oldWr:" + wr);

				ts = event.getTs();
				wr = event.getWr();
				val = event.getV();
			} 
//			else
//			{
//				logger.debug("Will not write " + event.getV() + " ts:" + event.getTs() + " oldTs:" + ts + " wr:" + event.getWr() + " oldWr:" + wr);
//			}

			RIWCMAckMessage ackMessage = new RIWCMAckMessage(selfAddress, event.getR());
			Pp2pSend sendEvent = new Pp2pSend(event.getSource(), ackMessage);
			trigger(sendEvent, pp2p);
		}
	};

	Handler<RIWCMAckMessage> riwcmAckDeliverHandler = new Handler<RIWCMAckMessage>()
	{

		@Override
		public void handle(RIWCMAckMessage event)
		{
			if (event.getR() == rid)
			{
//				logger.debug("RIWCMAckMessage from process" + event.getSource().getId());

				acks++;

				if (acks > (allProcesses.size() / 2))
				{
					acks = 0;

					if (reading)
					{
						reading = false;
						trigger(new ArReadResponse(readVal), nnar);
					} else
					{
						trigger(new ArWriteResponse(), nnar);
					}
				}
			} 
//			else
//			{
//				logger.debug("RIWCMAckMessage from process" + event.getSource().getId() + ", R not matching");
//			}
		}
	};

	private static class Holder
	{
		int ts = 0;
		int wr = 0;
		int val = 0;

		public Holder(int ts, int wr, int val)
		{
			this.ts = ts;
			this.wr = wr;
			this.val = val;
		}
	}

}
