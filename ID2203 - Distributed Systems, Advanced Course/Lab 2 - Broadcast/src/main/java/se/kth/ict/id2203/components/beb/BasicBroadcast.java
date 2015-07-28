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
package se.kth.ict.id2203.components.beb;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

public class BasicBroadcast extends ComponentDefinition
{

	@SuppressWarnings("unused")
	private static final Logger logger = LoggerFactory.getLogger(BasicBroadcast.class);

	private Positive<PerfectPointToPointLink> pp2pPort = requires(PerfectPointToPointLink.class);
	private Negative<BestEffortBroadcast> bebPort = provides(BestEffortBroadcast.class);

	private Address selfAddress;
	private List<Address> allProcesses = new ArrayList<>();


	public BasicBroadcast(BasicBroadcastInit init)
	{
		selfAddress = init.getSelfAddress();
		allProcesses.addAll(init.getAllAddresses());

		subscribe(bebBroadcastHandler, bebPort);
		subscribe(pp2pDeliverHandler, pp2pPort);
	}

	Handler<BebBroadcast> bebBroadcastHandler = new Handler<BebBroadcast>()
	{

		@Override
		public void handle(BebBroadcast event)
		{
			// logger.debug("Sending pp2p broadcast type " +
			// event.getClass().getSimpleName() + " deliver type " +
			// event.getDeliverEvent().getClass().getSimpleName());

			for (Address address : allProcesses)
			{
				Pp2pDeliver deliverEvent = new BebDataMessage(selfAddress, event.getDeliverEvent());
				Pp2pSend sendEvent = new Pp2pSend(address, deliverEvent);
				trigger(sendEvent, pp2pPort);
			}
		}

	};

	Handler<Pp2pDeliver> pp2pDeliverHandler = new Handler<Pp2pDeliver>()
	{

		@Override
		public void handle(Pp2pDeliver event)
		{
			// logger.debug("Pp2pDeliver from " + event.getSource().getId() +
			// " type = " + event.getClass().getSimpleName());

			if (event instanceof BebDataMessage)
			{
				BebDataMessage bebDataMessage = (BebDataMessage) event;

				trigger(bebDataMessage.getDeliverEvent(), bebPort);
			}
		}
	};

}
