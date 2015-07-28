package se.mattec.id2209.hw1.agents;

import jade.core.AID;
import jade.core.Agent;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREInitiator;
import jade.proto.states.MsgReceiver;

import java.io.IOException;

import se.mattec.id2209.hw1.models.RequestIdentifiers;

@SuppressWarnings("serial")
public class TourGuideAgent extends Agent {

	@Override
	protected void setup() {
		super.setup();
		System.out.println("TourGuideAgent started, name: " + getLocalName());
		
		//Register guide services
		registerService();

		//Add MsgReceiver for tour request messages
		addMsgReceiver();
	}
	
	/**
	 * Adds a MsgReceiver behaviour to receive tour requests.
	 * */
	private void addMsgReceiver() {
		//Receiving requests for tour from profiler
		MessageTemplate tourRequestMessageTemplate = MessageTemplate.MatchOntology(RequestIdentifiers.REQUEST_TOUR);
		addBehaviour(new MsgReceiver(this, tourRequestMessageTemplate, Long.MAX_VALUE, null, null) {
			
			@Override
			protected void handleMessage(ACLMessage msg) {
				super.handleMessage(msg);
				System.out.println("Guide received tour request from profiler");
				
				//Requests the curator to build a tour
				ACLMessage message = new ACLMessage(ACLMessage.REQUEST);
				message.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST); 
				message.addReceiver(findCurator(getName()));
				message.setOntology(RequestIdentifiers.REQUEST_BUILD_TOUR);
				
				try {
					message.setContentObject(msg.getContentObject());
				} 
				catch (IOException | UnreadableException e) {
					e.printStackTrace();
				}
				
				addBehaviour(new BuildTourInitiator(TourGuideAgent.this, message, msg));
			}
			
			@Override
			public int onEnd() {
				//Re-add MsgReceiver
				addMsgReceiver();
				return super.onEnd();
			}
		});
	}
	
	/**
	 * Registers guide services to DF.
	 * */
	private void registerService() {
		DFAgentDescription description = new DFAgentDescription();
		description.setName(getAID());
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("guide");
		serviceDescription.setName(getName());
		description.addServices(serviceDescription);
		try {
			DFService.register(this, description);
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Search for a curator based on the tour name.
	 * */
	private AID findCurator(String tourName) {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("curator");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this,  description);
			if (resultAgentDescriptions.length > 0) {
				return resultAgentDescriptions[0].getName();
			}
		} 
		catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Initiator for build tour message to curator.
	 * Will forward reply to the profiler who requested the tour.
	 * */
	class BuildTourInitiator extends SimpleAchieveREInitiator {
		ACLMessage original;
		
		public BuildTourInitiator(Agent a, ACLMessage msg, ACLMessage original) {
			super(a, msg);
			this.original = original;
		}

		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);
			System.out.println("Guide received tour built from curator");

			//Sending response back to profiler
			ACLMessage reply = original.createReply();
			reply.setPerformative(ACLMessage.INFORM);
			
			try {
				reply.setContentObject(msg.getContentObject());
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			send(reply);
		}
	}

}
