package se.mattec.id2209.hw3.behaviours;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.core.behaviours.ParallelBehaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;

import java.awt.Frame;
import java.awt.Label;
import java.awt.Panel;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;

import se.mattec.id2209.hw3.models.Artifact;
import se.mattec.id2209.hw3.models.Auction;
import se.mattec.id2209.hw3.models.AuctionStatusEnum;

@SuppressWarnings("serial")
public class AuctioneerBehaviour extends ParallelBehaviour {
	CyclicBehaviour c;
	
	private Auction auction;

	private List<AID> participants;

	private int minPrice;
	private int decreasingSize;
	
	private int sellPrice;

	public AuctioneerBehaviour(Agent agent, Artifact artifact, int realPrice) {		
		super(agent, ParallelBehaviour.WHEN_ALL);

		// Initiate the auction
		auction = new Auction();
		auction.setId((long) (Math.random() * 1000));
		auction.setArtifact(artifact);
		auction.setStatus(AuctionStatusEnum.NOT_STARTED);
		auction.setCurrentBid(realPrice * 2);
		auction.setRealPrice(realPrice);
		auction.setStartTime(System.currentTimeMillis());

		this.minPrice = (int) (realPrice * 0.8);
		this.decreasingSize = realPrice / 20;

		this.participants = findPotentialParticipants();

		System.out.println(myAgent.getLocalName() + ": Start of auction - " + auction.getCurrentBid() + " - " + auction.getId());

		sendInformStartOfAuction();

		// Add a ticker behaviour that will propose a new bid every second.
		addSubBehaviour(new TickerBehaviour(myAgent, 1000) {

			@Override
			protected void onTick() {
				// Ticker will be canceled if the auctions status is not ongoing.
				if (auction.getStatus() != AuctionStatusEnum.ONGOING) {
					System.out.println(myAgent.getLocalName() + ": End of auction - " + auction.getCurrentBid() + " - " + auction.getId());

					sendInformEnded();
					stop();
					removeSubBehaviour(c);

					showAuctionDetailsGUI();

					return;
				}

				int newBid = auction.getCurrentBid() - decreasingSize;

				// Send new bids to participants if it is not below the minimum price.
				if (newBid >= minPrice) {
					auction.setCurrentBid(newBid);

					System.out.println(myAgent.getLocalName() + ": Sending new bid in auction - " + auction.getCurrentBid() + " - " + auction.getId());

					sendNewBid();
				}
				// If bid is below minimum price, inform the participants the auction ended without bids.
				else {
					System.out.println(myAgent.getLocalName() + ": End of auction without bids - " + auction.getCurrentBid() + " - " + auction.getId());

					auction.setStatus(AuctionStatusEnum.ENDED_NOT_SOLD);
					sendInformNoBids();
				}
			}
		});

		c = new CyclicBehaviour() {

			@Override
			public void action() {
				MessageTemplate mt = MessageTemplate.MatchOntology("AUCTION");
				ACLMessage message = myAgent.receive(mt);

				if (message != null) {

					switch (message.getPerformative()) {

					// Propose message received. This is received by auctioneer when a buyer accepted a bid.
					case ACLMessage.PROPOSE: {
						Auction auctionFromBuyer = null;

						try {
							auctionFromBuyer = (Auction) message.getContentObject();
						}
						catch (UnreadableException e1) {
							e1.printStackTrace();
						}

						if (auctionFromBuyer != null) {
							// If the auction didnt end already (someone else made a propose first)
							if (auction.getStatus() == AuctionStatusEnum.ONGOING) {
								//If the propose is for the current bid, accept it and end the auction.
								if (auctionFromBuyer.getCurrentBid() == auction.getCurrentBid()) {
									System.out.println(myAgent.getLocalName() + ": Proposal from bidder accepted - " + auction.getCurrentBid() + " - " + auction.getId());

									auction.setStatus(AuctionStatusEnum.ENDED_SOLD);
									auction.setBuyerName(message.getSender().getLocalName());
									
									sellPrice = auction.getCurrentBid();

									ACLMessage reply = message.createReply();
									reply.setPerformative(ACLMessage.ACCEPT_PROPOSAL);

									try {
										reply.setContentObject(auction);
									} 
									catch (IOException e) {
										e.printStackTrace();
									}

									reply.setOntology("AUCTION");
									myAgent.send(reply);
								}
								// Late proposals are not allowed, reject.
								else {
									System.out.println(myAgent.getLocalName() + ": Proposal from bidder rejected - " + auction.getCurrentBid() + " - " + auction.getId());

									ACLMessage reply = message.createReply();
									reply.setPerformative(ACLMessage.REJECT_PROPOSAL);

									try {
										reply.setContentObject(auction);
									} 
									catch (IOException e) {
										e.printStackTrace();
									}

									reply.setOntology("AUCTION");
									myAgent.send(reply);
								}
							}
							// If the auction already ended send a reject message.
							else {
								System.out.println(myAgent.getLocalName() + ": Proposal from bidder rejected - " + auction.getCurrentBid() + " - " + auction.getId());

								ACLMessage reply = message.createReply();
								reply.setPerformative(ACLMessage.REJECT_PROPOSAL);

								try {
									reply.setContentObject(auction);
								} 
								catch (IOException e) {
									e.printStackTrace();
								}

								reply.setOntology("AUCTION");
								myAgent.send(reply);
							}
						}
						else {
							System.out.println(myAgent.getLocalName() + ": No auction received from buyer");
						}
						break;
					}

					default: {
						break;
					}
					}
				}
				else {
					block();
				}
			}
		};
		
		addSubBehaviour(c);

		myAgent.addBehaviour(this);
	}

	/**
	 * Inform all participants that the action ended without any bids.
	 * */
	private void sendInformNoBids() {
		ACLMessage message = new ACLMessage(ACLMessage.INFORM);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		auction.setNote("NO_BIDS");
		try {
			message.setContentObject(auction);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology("AUCTION");
		myAgent.send(message);
	}

	/**
	 * Inform all participants that the auction ended.
	 * */
	private void sendInformEnded() {
		ACLMessage message = new ACLMessage(ACLMessage.INFORM);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		auction.setNote("ENDED");
		try {
			message.setContentObject(auction);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology("AUCTION");
		myAgent.send(message);
	}

	/**
	 * Informs all participants of the start of the auction for the artifact.
	 * Also sets status to ongoing.
	 * */
	private void sendInformStartOfAuction() {
		auction.setStatus(AuctionStatusEnum.ONGOING);

		ACLMessage message = new ACLMessage(ACLMessage.INFORM);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		auction.setNote("START");
		try {
			message.setContentObject(auction);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology("AUCTION");
		myAgent.send(message);
	}

	/**
	 * Sends a bid to all participants.
	 * */
	private void sendNewBid() {
		ACLMessage message = new ACLMessage(ACLMessage.CFP);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		try {
			message.setContentObject(auction);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology("AUCTION");
		myAgent.send(message);
	}

	/**
	 * Find all registered participants.
	 * */
	private List<AID> findPotentialParticipants() {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType(myAgent.here().getName() + "participant");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(myAgent, description);
			List<AID> agents = new ArrayList<AID>();
			for (int i = 0; i < resultAgentDescriptions.length; i++) {
				agents.add(resultAgentDescriptions[i].getName());
			}
			return agents;
		} 
		catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Creates a simple gui to show the results of an auction
	 * */
	private void showAuctionDetailsGUI() {
		final Frame frame = new Frame("Auction details");

		Panel panel = new Panel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		Label idLabel = new Label("Auction id: " + auction.getId());
		panel.add(idLabel);

		if (auction.getStatus() == AuctionStatusEnum.ENDED_SOLD) {
			Label buyerLabel = new Label("Buyer name: " + auction.getBuyerName());
			panel.add(buyerLabel);
		}
		else {
			Label notSoldLabel = new Label("Item was not sold.");
			panel.add(notSoldLabel);
		}

		Label priceLabel = new Label("Price: " + auction.getCurrentBid());
		panel.add(priceLabel);

		long seconds = (System.currentTimeMillis() - auction.getStartTime()) / 1000;
		Label timeLabel = new Label("Auction time: " + seconds + " seconds");
		panel.add(timeLabel);

		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent we){
				frame.dispose();
			}
		});
	}

	public int getSellPrice() {
		return sellPrice;
	}

}