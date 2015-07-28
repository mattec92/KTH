package se.mattec.id2209.hw3.agents;

import jade.content.ContentElement;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.core.AID;
import jade.core.Agent;
import jade.core.Location;
import jade.core.behaviours.CyclicBehaviour;
import jade.core.behaviours.OneShotBehaviour;
import jade.core.behaviours.SequentialBehaviour;
import jade.domain.JADEAgentManagement.QueryPlatformLocationsAction;
import jade.domain.mobility.MobilityOntology;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import java.util.HashMap;

import se.mattec.id2209.hw3.behaviours.AuctioneerBehaviour;
import se.mattec.id2209.hw3.models.Artifact;

@SuppressWarnings("serial")
public class ArtistManagerAgent extends Agent {
	private static final int PRICE = 1000;
	
	private HashMap<String, Location> locations;
	private Location home;
	private AID originalAgent;
	private HashMap<String, Integer> sellPrices;

	@Override
	protected void setup() {
		super.setup();
		System.out.println("ArtistManagerAgent started, name: " + getLocalName());

		//Setup, save original agents AID and container to return to.
		locations = new HashMap<String, Location>();
		sellPrices = new HashMap<String, Integer>();
		originalAgent = getAID();
		home = here();

		getContentManager().registerLanguage(new SLCodec());
		getContentManager().registerOntology(MobilityOntology.getInstance());

		// Get available locations with AMS
		sendRequest(new Action(getAMS(), new QueryPlatformLocationsAction()));

		try {
			//Receive response from AMS
			MessageTemplate mt = MessageTemplate.and(
					MessageTemplate.MatchSender(getAMS()),
					MessageTemplate.MatchPerformative(ACLMessage.INFORM));
			ACLMessage resp = blockingReceive(mt);
			ContentElement ce = getContentManager().extractContent(resp);
			Result result = (Result) ce;
			jade.util.leap.Iterator it = result.getItems().iterator();

			while (it.hasNext()) {
				Location loc = (Location)it.next();
				locations.put(loc.getName(), loc);
			}
			locations.remove(here().getName());
			locations.remove("Main-Container");
		}
		catch (Exception e) { 
			e.printStackTrace(); 
		}
		
		doWait(10000);

		//Clone agents so there is one agent for each container.
		int cloneNumber = 1;
		SequentialBehaviour cloningBehaviour = new SequentialBehaviour();
		for (final Location location : locations.values()) {
			final String cloneName = getLocalName() + " clone " + cloneNumber;
			cloningBehaviour.addSubBehaviour(new OneShotBehaviour() {

				@Override
				public void action() {
					if (getLocalName().contains("clone") == false) {
						doClone(location, cloneName);
					}
				}
			});
			cloneNumber++;
		}
		addBehaviour(cloningBehaviour);

		//Add behaviour to receive end of auction information about prices.
		addBehaviour(new CyclicBehaviour() {

			@Override
			public void action() {
				MessageTemplate mt = MessageTemplate.MatchOntology("END_OF_AUCTION");
				ACLMessage message = receive(mt);

				if (message != null) {
					int sellPrice = Integer.parseInt(message.getContent());
					sellPrices.put(message.getSender().getLocalName(), sellPrice);

					if (sellPrices.size() == locations.size()) {
						int highestPrice = Integer.MIN_VALUE;
						for (int price : sellPrices.values()) {
							if (price > highestPrice) {
								highestPrice = price;
							}
						}
						System.out.println("Highest price for auction was: " + highestPrice);
					}
				}
				else {
					block();
				}
			}
		});
	}

	/**
	 * Used to send a request to get the containers...?
	 * */
	void sendRequest(Action action) {

		ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
		request.setLanguage(new SLCodec().getName());
		request.setOntology(MobilityOntology.getInstance().getName());
		try {
			getContentManager().fillContent(request, action);
			request.addReceiver(action.getActor());
			send(request);
		}
		catch (Exception ex) { ex.printStackTrace(); }
	}

	/**
	 * Invoked after cloning. Adding the AuctioneerBehaviour and overriding onEnd
	 * to send a message to the original agent to share its price.
	 * Also moves the agent to the home container.
	 * */
	@Override
	protected void afterClone() {
		super.afterClone();
		addBehaviour(new AuctioneerBehaviour(ArtistManagerAgent.this, new Artifact(), PRICE) {

			@Override
			public int onEnd() {
				ACLMessage message = new ACLMessage(ACLMessage.INFORM);
				message.setOntology("END_OF_AUCTION");
				message.addReceiver(originalAgent);
				message.setContent(String.valueOf(getSellPrice()));
				send(message);

				doMove(home);

				return super.onEnd();
			}
		});
	}

	/**
	 * Deletes the agent after it moved back to the home container. (After a while)
	 * */
	@Override
	protected void afterMove() {
		super.afterMove();
		doWait(5000);
		doDelete();
	}

}
