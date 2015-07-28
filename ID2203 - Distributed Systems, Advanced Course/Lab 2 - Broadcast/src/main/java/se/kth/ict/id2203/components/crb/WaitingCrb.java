/**
 * This file is part of the ID2203 course assignments kit.
 * 
 * Copyright (C) 2009-2013 KTH Royal Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package se.kth.ict.id2203.components.crb;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import se.kth.ict.id2203.ports.crb.CausalOrderReliableBroadcast;
import se.kth.ict.id2203.ports.crb.CrbBroadcast;
import se.kth.ict.id2203.ports.rb.RbBroadcast;
import se.kth.ict.id2203.ports.rb.RbDeliver;
import se.kth.ict.id2203.ports.rb.ReliableBroadcast;
import se.sics.kompics.ComponentDefinition;
import se.sics.kompics.Handler;
import se.sics.kompics.Negative;
import se.sics.kompics.Positive;
import se.sics.kompics.address.Address;

public class WaitingCrb extends ComponentDefinition
{

	@SuppressWarnings("unused")
	private static final Logger logger = LoggerFactory.getLogger(WaitingCrb.class);

	private Positive<ReliableBroadcast> rbPort = requires(ReliableBroadcast.class);
	private Negative<CausalOrderReliableBroadcast> crbPort = provides(CausalOrderReliableBroadcast.class);

	private Address selfAddress;
	private List<Address> allProcesses = new ArrayList<>();

	private Map<Integer, Integer> vector = new HashMap<Integer, Integer>();

	private int localSequenceNumber = 0;

	private HashSet<CrbDataMessage> pendingMessages = new HashSet<>();


	public WaitingCrb(WaitingCrbInit init)
	{
		selfAddress = init.getSelfAddress();
		allProcesses.addAll(init.getAllAddresses());

		for (Address address : allProcesses)
		{
			vector.put(address.getId(), 0);
		}

		subscribe(crbBroadcastHandler, crbPort);
		subscribe(rbDeliverHandler, rbPort);
	}

	Handler<CrbBroadcast> crbBroadcastHandler = new Handler<CrbBroadcast>()
	{

		@Override
		public void handle(CrbBroadcast event)
		{
			// logger.debug("Sending rb broadcast type " +
			// event.getClass().getSimpleName() + " deliver type " +
			// event.getDeliverEvent().getClass().getSimpleName());

			Map<Integer, Integer> newVector = new HashMap<>(vector);

			newVector.put(selfAddress.getId(), localSequenceNumber);

			localSequenceNumber++;

			RbDeliver deliverEvent = new CrbDataMessage(event.getDeliverEvent().getSource(), newVector,
					event.getDeliverEvent());
			RbBroadcast sendEvent = new RbBroadcast(deliverEvent);
			trigger(sendEvent, rbPort);
		}
	};

	Handler<RbDeliver> rbDeliverHandler = new Handler<RbDeliver>()
	{

		@Override
		public void handle(RbDeliver event)
		{
			// logger.debug("RBDeliver from " + event.getSource() + " type " +
			// event.getClass().getSimpleName());

			if (event instanceof CrbDataMessage)
			{
				CrbDataMessage crbDataMessage = (CrbDataMessage) event;

				pendingMessages.add(crbDataMessage);

				CrbDataMessage pendingMessage = null;
				while ((pendingMessage = findPendingMessage()) != null)
				{
					pendingMessages.remove(pendingMessage);

					int pendingMessageRank = pendingMessage.getSource().getId();
					int newVectorValue = vector.get(pendingMessageRank) + 1;
					vector.put(pendingMessageRank, newVectorValue);

					trigger(pendingMessage.getDeliverEvent(), crbPort);
				}
			}
		}
	};


	private CrbDataMessage findPendingMessage()
	{
		for (CrbDataMessage crbDataMessage : pendingMessages)
		{
			if (isLessOrEqual(crbDataMessage.getVector(), vector))
			{
				return crbDataMessage;
			}
		}

		return null;
	}


	/**
	 * Returns true if leftVector is strictly less or equal to rightVector.
	 * */
	private boolean isLessOrEqual(Map<Integer, Integer> leftVector, Map<Integer, Integer> rightVector)
	{
		boolean isLessOrEqual = true;

		for (Integer processId : leftVector.keySet())
		{
			if (leftVector.get(processId) > rightVector.get(processId))
			{
				isLessOrEqual = false;

				break;
			}
		}

		return isLessOrEqual;
	}

}
