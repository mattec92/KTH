package se.kth.ict.id2203.components.epfd;

import java.util.HashSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.ict.id2203.ports.epfd.EventuallyPerfectFailureDetector;
import se.kth.ict.id2203.ports.epfd.Restore;
import se.kth.ict.id2203.ports.epfd.Suspect;
import se.kth.ict.id2203.ports.pp2p.PerfectPointToPointLink;
import se.kth.ict.id2203.ports.pp2p.Pp2pDeliver;
import se.kth.ict.id2203.ports.pp2p.Pp2pSend;
import se.sics.kompics.ComponentDefinition;
import se.sics.kompics.Handler;
import se.sics.kompics.Negative;
import se.sics.kompics.Positive;
import se.sics.kompics.Start;
import se.sics.kompics.address.Address;
import se.sics.kompics.timer.ScheduleTimeout;
import se.sics.kompics.timer.Timer;

public class Epfd extends ComponentDefinition
{

	private static final Logger logger = LoggerFactory.getLogger(Epfd.class);

	private long delay;
	private long delta;

	private Negative<EventuallyPerfectFailureDetector> epfdPort = provides(EventuallyPerfectFailureDetector.class);
	private Positive<PerfectPointToPointLink> pp2pPort = requires(PerfectPointToPointLink.class);
	private Positive<Timer> timerPort = requires(Timer.class);

	private Address selfAddress;

	private HashSet<Address> allProcesses = new HashSet<>();
	private HashSet<Address> alive = new HashSet<>();
	private HashSet<Address> suspected = new HashSet<>();

	private int sequenceNumber = 0;


	public Epfd(EpfdInit init)
	{
		delay = init.getInitialDelay();
		delta = init.getDeltaDelay();

		selfAddress = init.getSelfAddress();

		allProcesses.addAll(init.getAllAddresses());
		alive.addAll(init.getAllAddresses());

		subscribe(startHandler, control);
		subscribe(timeoutHandler, timerPort);
		subscribe(heartbeatReplyHandler, pp2pPort);
		subscribe(heartbeatRequestHandler, pp2pPort);
	}

	Handler<Start> startHandler = new Handler<Start>()
	{

		@Override
		public void handle(Start startEvent)
		{
			ScheduleTimeout scheduleTimeout = new ScheduleTimeout(delay);
			scheduleTimeout.setTimeoutEvent(new CheckTimeout(scheduleTimeout));
			trigger(scheduleTimeout, timerPort);
		}

	};

	Handler<CheckTimeout> timeoutHandler = new Handler<CheckTimeout>()
	{

		@Override
		public void handle(CheckTimeout timeoutEvent)
		{
//			logger.debug("Received TimeOut");
			
			for (Address address : alive)
			{
				if (suspected.contains(address))
				{
					delay += delta;
					logger.debug("Delay increased to " + delay);
					break;
				}
			}

			sequenceNumber++;

			for (Address address : allProcesses)
			{
				if (!alive.contains(address) && !suspected.contains(address))
				{
					suspected.add(address);
					trigger(new Suspect(address), epfdPort);
				}
				else if (alive.contains(address) && suspected.contains(address))
				{
					suspected.remove(address);
					trigger(new Restore(address), epfdPort);
				}

				Pp2pDeliver deliverEvent = new HeartbeatRequestMessage(selfAddress, sequenceNumber);
				Pp2pSend sendEvent = new Pp2pSend(address, deliverEvent);
				trigger(sendEvent, pp2pPort);
			}

			alive.clear();

			ScheduleTimeout scheduleTimeout = new ScheduleTimeout(delay);
			scheduleTimeout.setTimeoutEvent(new CheckTimeout(scheduleTimeout));
			trigger(scheduleTimeout, timerPort);
		}

	};

	Handler<HeartbeatRequestMessage> heartbeatRequestHandler = new Handler<HeartbeatRequestMessage>()
	{

		@Override
		public void handle(HeartbeatRequestMessage message)
		{
//			logger.debug("Received HeartbeatRequestMessage from " + message.getSource() + " Sn: " + sequenceNumber);
			
			Pp2pDeliver deliverEvent = new HeartbeatReplyMessage(selfAddress, message.getSequenceNumber());
			Pp2pSend sendEvent = new Pp2pSend(message.getSource(), deliverEvent);
			trigger(sendEvent, pp2pPort);
		}

	};

	Handler<HeartbeatReplyMessage> heartbeatReplyHandler = new Handler<HeartbeatReplyMessage>()
	{

		@Override
		public void handle(HeartbeatReplyMessage message)
		{
//			logger.debug("Received HeartbeatReplyMessage from " + message.getSource() + " Sn: " + sequenceNumber);
			
			if (message.getSequenceNumber() == sequenceNumber || suspected.contains(message.getSource()))
			{
				alive.add(message.getSource());
			}
		}

	};

}