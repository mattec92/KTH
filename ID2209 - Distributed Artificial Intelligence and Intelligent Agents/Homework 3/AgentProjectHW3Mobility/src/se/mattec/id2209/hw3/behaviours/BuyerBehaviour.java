package se.mattec.id2209.hw3.behaviours;

import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;

import java.io.IOException;

import se.mattec.id2209.hw3.models.Auction;
import se.mattec.id2209.hw3.models.AuctionStatusEnum;

@SuppressWarnings("serial")
public class BuyerBehaviour extends CyclicBehaviour {
	private double priceWeight;

	public BuyerBehaviour(Agent agent, double priceWeight, boolean register) {
		super(agent);

		this.priceWeight = priceWeight;

		if (register) {
			registerAsParticipant();
		}
	}

	@Override
	public void action() {
		MessageTemplate mt = MessageTemplate.MatchOntology("AUCTION");
		ACLMessage message = myAgent.receive(mt);

		if (message != null) {
			Auction auction = null;

			try {
				auction = (Auction) message.getContentObject();						
			} 
			catch (UnreadableException e1) {
				e1.printStackTrace();
			}

			if (auction != null) {
				switch (message.getPerformative()) {

				case ACLMessage.INFORM: {
					//Inform messages, informing of start or end of auction.
					if (auction.getNote().equals("START")) {
						System.out.println(myAgent.getLocalName() + ": Start of auction - " + auction.getCurrentBid() + " - " + auction.getId());
					}
					else if (auction.getNote().equals("ENDED")) {
						System.out.println(myAgent.getLocalName() + ": End of auction - " + auction.getCurrentBid() + " - " + auction.getId());
					}
					else if (auction.getNote().equals("NO_BIDS")) {
						System.out.println(myAgent.getLocalName() + ": End of auction without bids - " + auction.getCurrentBid() + " - " + auction.getId());
					}

					break;
				}

				case ACLMessage.CFP: {
					if (auction.getStatus() == AuctionStatusEnum.ONGOING) {
						//If the bid proposed by the auctioneer is below the buyers max price, propose to buy it.
						if (auction.getCurrentBid() <= (auction.getRealPrice() * priceWeight)) {
							System.out.println(myAgent.getLocalName() + ": Making a proposal to buy item - " + auction.getCurrentBid() + " - " + auction.getId());

							ACLMessage reply = message.createReply();
							reply.setPerformative(ACLMessage.PROPOSE);

							try {
								reply.setContentObject(auction);
							} 
							catch (IOException e) {
								e.printStackTrace();
							}

							reply.setOntology("AUCTION");
							myAgent.send(reply);
						}
						//Otherwise dont accept it. (Do nothing)
						else {
							System.out.println(myAgent.getLocalName() + ": Not accepting bid in auction - " + auction.getCurrentBid() + " - " + auction.getId());
						}
					}
					break;
				}

				case ACLMessage.ACCEPT_PROPOSAL: {
					// The bid the buyer proposed was accepted.
					System.out.println(myAgent.getLocalName() + ": Proposal accepted in auction - " + auction.getCurrentBid() + " - " + auction.getId());
					break;
				}

				case ACLMessage.REJECT_PROPOSAL: {
					// The bid the buyer proposed was rejected.
					System.out.println(myAgent.getLocalName() + ": Proposal rejected in auction - " + auction.getCurrentBid() + " - " + auction.getId());
					break;
				}

				default: {
					break;
				}
				}
			}
			else {
				System.out.println(myAgent.getLocalName() + ": No auction specified");
			}
		}
		else {
			block();
		}
	}

	/**
	 * Registers interest in participating in auctions.
	 * */
	private void registerAsParticipant() {
		DFAgentDescription description = new DFAgentDescription();
		description.setName(myAgent.getAID());
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("participant");
		serviceDescription.setName(myAgent.getName());
		description.addServices(serviceDescription);
		try {
			DFService.register(myAgent, description);
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}
}